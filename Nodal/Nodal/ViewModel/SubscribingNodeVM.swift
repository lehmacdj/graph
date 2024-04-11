//
//  SubscribingNodeVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 8/6/23.
//

import Foundation
import Observation
import Combine


@Observable final class SubscribingNodeVM<N: Node>: NodeVM {
    let nid: NID

    var state: Loading<any NodeState<N>> {
        switch internalState {
        case .idle:
            return .idle
        case .loadingActive(state: nil, _):
            return .loading
        case .loadingActive(state: .some(let state), _):
            // even though we're internally loading this indicates that we were fully loaded before & thus we return the loaded information we already had
            return .loaded(state)
        case .loadingInactive:
            return .loading
        case .loadedActive(let state, _):
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
        /// `loadingActive` captures a few substates all of which are valid
        /// * the state is stored if we transitioned to loading from a loaded state so that we can still display it to the user
        /// * the node is populated during the loading process and is expected to exist for the final stage (initial data fetch) of the loading process
        case loadingActive(state: State?, node: N?)
        case loadingInactive
        case loadedActive(state: State, node: N)
        case loadedInactive(state: State)
        case failed(error: Error)

        var node: N? {
            switch self {
            case .idle, .loadingInactive, .loadedInactive, .failed:
                return nil
            case .loadingActive(_, let node):
                return node
            case .loadedActive(_, let node):
                return node
            }
        }

        var validStateForUpdate: (State?, N)? {
            switch self {
            case .idle, .loadingInactive, .loadedInactive, .failed, .loadingActive(_, nil):
                return nil
            case .loadingActive(let state, let .some(node)):
                return (state, node)
            case .loadedActive(let state, let node):
                return (state, node)
            }
        }
    }

    fileprivate struct TransitionKey: Hashable {
        let transition: NodeTransition
        let direction: Direction

        init(_ transition: NodeTransition, direction: Direction) {
            self.transition = transition
            self.direction = direction
        }

        init(transition: String, destination: NID, direction: Direction) {
            self.transition = NodeTransition(transition: transition, nid: destination)
            self.direction = direction
        }
    }

    struct State: NodeState {
        var data: Data?

        /// Map from destination NID to the corresponding TransitionVM
        fileprivate var destinationVMs: [TransitionKey: AnyTransitionVM<N>]

        var favoriteLinksTransitions: [NodeTransition]?
        var linksTransitions: [NodeTransition]
        var worseLinksTransitions: [NodeTransition]?
        var backlinksTransitions: [NodeTransition]

        var favoriteLinks: [AnyTransitionVM<N>]? {
            favoriteLinksTransitions?.compactMap { destinationVMs[TransitionKey($0, direction: .forward)] }
        }

        var links: [AnyTransitionVM<N>] {
            linksTransitions.compactMap { destinationVMs[TransitionKey($0, direction: .forward)] }
        }

        var worseLinks: [AnyTransitionVM<N>]? {
            worseLinksTransitions?.compactMap { destinationVMs[TransitionKey($0, direction: .forward)] }
        }

        var backlinks: [AnyTransitionVM<N>] {
            backlinksTransitions.compactMap { destinationVMs[TransitionKey($0, direction: .backward)] }
        }

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
        case .idle, .loadingActive, .loadingInactive, .loadedInactive, .failed:
            throw invalidState(expected: .loadedActive)
        case .loadedActive(_, let node):
            // we could theoretically expand this to also cover the case when we're currently loading, but I don't think that solves any problems and just makes concurrency a little harder to reason about
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

    /// Called when the NodeVM should subscribe to the underlying Node/filesystem events.
    /// The task is cancelled when it the Node should be deallocated.
    func subscribe() async {
        logInfo("\(nid) starting")
        do {
            while true {
                try await beginUpdateState()
                try Task.checkCancellation()
            }
        } catch is CancellationError {
        } catch {
            logError("unexpected error thrown \(error)")
            internalState = .failed(error: error)
        }

        switch internalState {
        case .loadingActive(state: nil, _):
            internalState = .loadingInactive
        case .loadingActive(let .some(state), _), .loadedActive(let state, _):
            internalState = .loadedInactive(state: state)
        case .idle, .loadingInactive, .loadedInactive, .failed:
            // nothing to do, already in an okay state
            break
        }
        logInfo("\(nid) done")
    }

    struct DuplicateSubscription: LocalizedError, Codable {}

    private func beginUpdateState() async throws {
        switch internalState {
        case .idle:
            internalState = .loadingActive(state: nil, node: nil)
        case .loadingInactive:
            internalState = .loadingActive(state: nil, node: nil)
        case .loadedInactive(let state):
            internalState = .loadingActive(state: state, node: nil)
        case .failed(_):
            // we don't want to start loading failed nodes so that the error message doesn't get cleared
            // we sleep for 1 second and then check the current state again, so that the VM can get unstuck if internalState gets set to something other than failed
            try await Task.sleep(for: .seconds(1))
            return
        case .loadingActive, .loadedActive:
            // Swift UI seems to call this multiple times so we need to be able to exit in this case
            throw DuplicateSubscription()
        }

        let node: N
        do {
            node = try await manager[nid]
        } catch {
            logError(error.localizedDescription)
            internalState = .failed(error: error)
            return
        }

        guard case .loadingActive(let state, let existingNode) = internalState else {
            return
        }

        guard existingNode == nil else {
            fatalError("it shouldn't be possible for node to be non-nil there should only be a single Task running beginUpdateState")
        }

        internalState = .loadingActive(state: state, node: node)

        try await updateStateLoop()
    }

    private func updateStateLoop() async throws {
        guard case .loadingActive(_, let .some(node)) = internalState else { return }

        for await _ in node.metaPublisher.values {
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
                transitions.sorted(on: {"\($0.transition)\($0.nid)"}).map {
                    SubscribingTransitionVM(parent: self, source: node, transition: $0, direction: direction, manager: self.manager, isFavorite: isFavorite, isWorse: isWorse)
                        .eraseToAnyTransitionVM()
                }
            }

            logInfo("about to create state")
            try Task.checkCancellation()

            guard let (previousState, node) = internalState.validStateForUpdate else {
                // we need to rerun beginUpdateState because the state has drifted to an invalid one while doing IO
                return
            }

            let favoriteLinks: [AnyTransitionVM<N>]? = favoritesSet != nil ? mkTransitionVMs(favorites, inDirection: .forward, isFavorite: true, isWorse: false) : nil
            let links: [AnyTransitionVM<N>] = mkTransitionVMs(normal, inDirection: .forward, isFavorite: false, isWorse: false)
            let worseLinks: [AnyTransitionVM<N>]? = worseSet != nil ? mkTransitionVMs(worse, inDirection: .forward, isFavorite: false, isWorse: true) : nil
            let backlinks: [AnyTransitionVM<N>] = mkTransitionVMs(node.incoming, inDirection: .backward, isFavorite: false, isWorse: false)

            let allDestinationVMs = (favoriteLinks ?? []) + links + (worseLinks ?? []) + backlinks
            var newDestinationVMs = allDestinationVMs
                .map { (TransitionKey(transition: $0.transition, destination: $0.destinationNid, direction: $0.direction), $0) }
                .to(Dictionary.init(uniqueKeysWithValues:))

            if let previousState {
                let newDestinationNids = newDestinationVMs.keys
                // prefer a TransitionVM that we previously created if it exists
                // NOTE: this potentially introduces a bug where the isFavorite/isWorse value could get out of sync
                // this seems sufficiently unlikely that it's okay until it becomes a problem
                newDestinationVMs = previousState
                    .destinationVMs
                    .filter { key, _ in newDestinationNids.contains(key) }
                    .map { key, value in (key, value) }
                    .to(Dictionary.init(uniqueKeysWithValues:))
                    .merging(newDestinationVMs) { key1, _ in key1 }
            }

            let favoriteLinksTransitions = favoriteLinks?
                .map { NodeTransition(transition: $0.transition, nid: $0.destinationNid) }
                .to(Array.init)
            let linksTransitions = links
                .map { NodeTransition(transition: $0.transition, nid: $0.destinationNid) }
                .to(Array.init)
            let worseLinksTransitions = worseLinks?
                .map { NodeTransition(transition: $0.transition, nid: $0.destinationNid) }
                .to(Array.init)
            let backlinksTransitions = backlinks
                .map { NodeTransition(transition: $0.transition, nid: $0.destinationNid) }
                .to(Array.init)

            let state = State(
                data: data,
                destinationVMs: newDestinationVMs,
                favoriteLinksTransitions: favoriteLinksTransitions,
                linksTransitions: linksTransitions,
                worseLinksTransitions: worseLinksTransitions,
                backlinksTransitions: backlinksTransitions,
                tags: tags,
                possibleTags: tagOptions
            )

            internalState = .loadedActive(state: state, node: node)

            logInfo("updated state")
        }
    }

    func reload() async {
        internalState = .loadingInactive
        // if subscribed, `updateStateLoop` will exit because of the unexpected state and `beginUpdateState` will rerun
    }
}
