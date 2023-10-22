//
//  SubscribingNodeVM.swift
//  view
//
//  Created by Devin Lehmacher on 8/6/23.
//

import Foundation
import Observation
import Combine


@Observable class SubscribingNodeVM<N: Node>: NodeVM {
    let nid: NID

    var state: Loading<any NodeState<N>> {
        switch internalState {
        case .idle:
            return .idle
        case .loadingActive:
            return .loading
        case .loadingInactive:
            return .loading
        case .loadingFromLoadedInactive(let state, _, _):
            return .loaded(state)
        case .loadedActive(let state, _, _):
            return .loaded(state)
        case .loadedInactive(let state):
            return .loaded(state)
        case .failed(let error):
            return .failed(error)
        }
    }

    private let manager: GraphManager<N>

    private var internalState: InternalState = .idle

    private enum InternalState {
        case idle
        case loadingActive(task: Task<Void, any Error>, semaphore: Semaphor)
        case loadingInactive
        case loadingFromLoadedInactive(state: State, task: Task<Void, any Error>, semaphore: Semaphor)
        case loadedActive(state: State, task: Task<Void, any Error>, node: N)
        case loadedInactive(state: State)
        case failed(error: Error)
    }

    struct State: NodeState {
        var data: Data?

        var favoriteLinks: [AnyTransitionVM<N>]?
        var links: [AnyTransitionVM<N>]
        var worseLinks: [AnyTransitionVM<N>]?
        var backlinks: [AnyTransitionVM<N>]

        var tags: Set<String>
        var possibleTags: Set<String>
    }

    private struct InvalidOperationForState: LocalizedError, Codable {
        enum StateDescription: String, Codable {
            case idle
            case loadingActive
            case loadingInactive
            case loadingFromLoadedInactive
            case loadedActive
            case loadedInactive
            case failed

            init(internalState: InternalState) {
                switch internalState {
                case .idle:
                    self = .idle
                case .loadingActive:
                    self = .loadingActive
                case .loadingInactive:
                    self = .loadingInactive
                case .loadingFromLoadedInactive:
                    self = .loadingFromLoadedInactive
                case .loadedActive:
                    self = .loadedActive
                case .loadedInactive:
                    self = .loadedInactive
                case .failed:
                    self = .failed
                }
            }
        }

        let currentState: StateDescription

        /// States from which the operation was legal
        let expectedState: [StateDescription]
    }

    private func invalidState(expected: InvalidOperationForState.StateDescription) -> InvalidOperationForState {
        InvalidOperationForState(currentState: .init(internalState: internalState), expectedState: [expected])
    }

    private func invalidState(expected: [InvalidOperationForState.StateDescription]) -> InvalidOperationForState {
        InvalidOperationForState(currentState: .init(internalState: internalState), expectedState: expected)
    }

    func set(tags: Set<String>) async throws {
        switch internalState {
        case .idle, .loadingActive, .loadingInactive, .loadingFromLoadedInactive, .loadedInactive, .failed:
            throw invalidState(expected: .loadedActive)
        case .loadedActive(_, _, let node):
            await node.set(tags: tags)
        }
    }

    func forceRemove() async {
        await manager.forceRemove(nid: nid)
    }

    func toggleFavorite(child _: NID) async throws {
    }

    func toggleWorse(child _: NID) async throws {
    }

    init(for nid: NID, in graph: GraphManager<N>) {
        self.nid = nid
        self.manager = graph
    }

    // TODO: move this stuff to a separate global VM for debug stats that we init at startup and store in the SwiftUI Environment
    var inMemoryNodeCount: Int = -1
    private var inMemoryNodeCountSubscription: AnyCancellable?

    func load() async {
        inMemoryNodeCountSubscription = await manager.mapTableSizePublisher.sink { [weak self] newValue in
            self?.inMemoryNodeCount = newValue
        }

        switch internalState {
        case .idle:
            internalState = .loadingInactive
            await loadState()
        case .loadingActive:
            // already in the process of loading
            return
        case .loadingInactive:
            // we need to start a new task because the previous one was cancelled
            await loadState()
        case .loadingFromLoadedInactive:
            // already in the process of loading
            return
        case .loadedActive(_, _, _):
            // already loaded, don't need to do anything
            return
        case .loadedInactive(_):
            await loadState()
        case .failed(_):
            // leave in failed state
            return
        }
    }

    private func loadState() async {
        let node: N
        do {
            node = try await manager[nid]
        } catch {
            logError(error.localizedDescription)
            internalState = .failed(error: error)
            return
        }

        let startSemaphore = Semaphor(initialCount: 0)
        let task = Task { [weak self, startSemaphore, node] in
            // wait until the internalState has been set appropriately to avoid race conditions
            await startSemaphore.wait()
            for await _ in node.metaPublisher.values {
                guard let self else { return }
                try Task.checkCancellation()

                // optimization opportunity: avoid using async for accessing some node properties, some of this data shouldn't require async, it's just a pure computation to get it from the node's metadata
                async let favoritesAsync = node.favorites
                async let worseAsync = node.worse
                async let dataAsync = node.data
                async let tagsAsync = node.tags
                async let possibleTagsAsync = self.manager.tags?.tagOptions
                let favoritesNode = await favoritesAsync
                let worseNode = await worseAsync
                let data = await dataAsync
                let tags = await tagsAsync
                let tagOptions = await possibleTagsAsync ?? Set()

                logInfo("done fetching data from node")
                try Task.checkCancellation()

                var favorites = [NodeTransition]()
                var normal = [NodeTransition]()
                var worse = [NodeTransition]()

                let favoritesSet: Set<NID>? = favoritesNode.map { Set($0.outgoing.map { $0.nid }) }
                let worseSet: Set<NID>? = worseNode.map { Set($0.outgoing.map { $0.nid }) }

                logInfo("creating children")
                try Task.checkCancellation()

                for child in node.outgoing {
                    if let favoritesSet, favoritesSet.contains(child.nid) {
                        favorites.append(child)
                    } else if let worseSet, worseSet.contains(child.nid) {
                        worse.append(child)
                    } else {
                        normal.append(child)
                    }
                }

                func mkTransitionVMs(_ transitions: [NodeTransition], inDirection direction: Direction, isFavorite: Bool, isWorse: Bool) -> [AnyTransitionVM<N>] {
                    transitions.sorted().map {
                        SubscribingTransitionVM(parent: self, source: node, transition: $0, direction: direction, manager: self.manager, isFavorite: isFavorite, isWorse: isWorse)
                            .eraseToAnyTransitionVM()
                    }
                }

                logInfo("about to create state")
                try Task.checkCancellation()

                let state = await State(
                    data: data?.data,
                    favoriteLinks: favoritesSet != nil ? mkTransitionVMs(favorites, inDirection: .forward, isFavorite: true, isWorse: false) : nil,
                    links: mkTransitionVMs(normal, inDirection: .forward, isFavorite: false, isWorse: false),
                    worseLinks: worseSet != nil ? mkTransitionVMs(worse, inDirection: .forward, isFavorite: false, isWorse: true) : nil,
                    backlinks: mkTransitionVMs(node.incoming, inDirection: .backward, isFavorite: false, isWorse: false),
                    tags: tags,
                    possibleTags: tagOptions
                )

                Task { @MainActor [weak self] in
                    guard let self else { return }
                    try Task.checkCancellation()

                    switch internalState {
                    case .idle, .loadingInactive, .loadedInactive, .failed:
                        // either the task should be cancelled thus would have thrown above or we should be in this state from below because we ensure that we don't start the enclosing task until after the internalState has been set
                        fatalError("these states should be impossible (if this fails, it would probably be okay to switch this to just return though)")
                    case .loadingActive(let task, let semaphore):
                        internalState = .loadedActive(state: state, task: task, node: node)
                        await semaphore.signal()
                    case .loadedActive(_, let task, _):
                        internalState = .loadedActive(state: state, task: task, node: node)
                    case .loadingFromLoadedInactive(_, let task, let semaphore):
                        internalState = .loadedActive(state: state, task: task, node: node)
                        await semaphore.signal()
                    }

                    logInfo("updated state")
                }

                logWarn("exited for loop fetching data")
            }
        }
        let semaphore = Semaphor(initialCount: 0)
        switch internalState {
        case .idle, .loadingActive, .loadedActive, .failed, .loadingFromLoadedInactive:
            internalState = .failed(error: invalidState(expected: [.loadingInactive, .loadedInactive]))
        case .loadingInactive:
            internalState = .loadingActive(task: task, semaphore: semaphore)
        case .loadedInactive(let state):
            internalState = .loadingFromLoadedInactive(state: state, task: task, semaphore: semaphore)
        }
        await startSemaphore.signal()
        await semaphore.wait()
    }

    func reload() async {
        switch internalState {
        case .idle, .loadedInactive, .failed:
            internalState = .loadingInactive
            await loadState()
        case .loadingActive(let task, let semaphore):
            task.cancel()
            await semaphore.signal()
            internalState = .loadingInactive
            await loadState()
        case .loadingInactive:
            // we're already in the correct state
            return
        case .loadingFromLoadedInactive(_, let task, let semaphore):
            task.cancel()
            await semaphore.signal()
            internalState = .loadingInactive
            await loadState()
        case .loadedActive(_, let task, _):
            task.cancel()
            internalState = .loadingInactive
            await loadState()
        }
    }

    func weaken() {
        switch internalState {
        case .idle:
            return
        case .loadingActive(let task, let semaphore):
            task.cancel()
            Task { await semaphore.signal() }
            internalState = .loadingInactive
        case .loadingInactive:
            return
        case .loadingFromLoadedInactive(let state, let task, let semaphore):
            task.cancel()
            Task { await semaphore.signal() }
            internalState = .loadedInactive(state: state)
        case .loadedActive(let state, let task, _):
            task.cancel()
            internalState = .loadedInactive(state: state)
        case .loadedInactive(_):
            return
        case .failed(_):
            return
        }
    }
}
