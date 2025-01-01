//
//  SubscribingNodeVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 8/6/23.
//

import Foundation
import Observation
import Combine
import AsyncAlgorithms


@Observable final class SubscribingNodeVM<N: Node>: NodeVM {
    let nid: NID

    var state: Loading<NodeState> {
        switch internalState {
        case .idle:
            return .idle
        case .loadingActive(state: nil, _):
            return .loading
        case .loadingActive(state: .some(let state), _):
            // even though we're internally loading this indicates that we were fully loaded before & thus we return the loaded information we already had
            return .loaded(state.toNodeState())
        case .loadingInactive:
            return .loading
        case .loadedActive(let state, _, _):
            return .loaded(state.toNodeState())
        case .loadedInactive(let state):
            return .loaded(state.toNodeState())
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
        case loadedActive(state: State, node: N, childNodeCancelSubjects: [NID: PassthroughSubject<(), Never>])
        case loadedInactive(state: State)
        case failed(error: Error)

        var node: N? {
            switch self {
            case .idle, .loadingInactive, .loadedInactive, .failed:
                return nil
            case .loadingActive(_, let node):
                return node
            case .loadedActive(_, let node, _):
                return node
            }
        }

        var validStateForUpdate: (State?, N, [NID: PassthroughSubject<(), Never>])? {
            switch self {
            case .idle, .loadingInactive, .loadedInactive, .failed, .loadingActive(_, nil):
                return nil
            case .loadingActive(let state, let .some(node)):
                return (state, node, [:])
            case .loadedActive(let state, let node, let childNodeCancelSubjects):
                return (state, node, childNodeCancelSubjects)
            }
        }
    }

    fileprivate struct State {
        var data: Data?

        /// Map from destination NID to the corresponding TransitionVM
        var destinationVMs: [TransitionKey: SubscribingTransitionVM<N>]

        var favoriteLinksTransitions: [NodeTransition]?
        var linksTransitions: [NodeTransition]
        var worseLinksTransitions: [NodeTransition]?
        var backlinksTransitions: [NodeTransition]

        var favoriteLinks: [SubscribingTransitionVM<N>]? {
            favoriteLinksTransitions?.compactMap { destinationVMs[TransitionKey($0, direction: .forward)] }
        }

        var links: [SubscribingTransitionVM<N>] {
            linksTransitions.compactMap { destinationVMs[TransitionKey($0, direction: .forward)] }
        }

        var worseLinks: [SubscribingTransitionVM<N>]? {
            worseLinksTransitions?.compactMap { destinationVMs[TransitionKey($0, direction: .forward)] }
        }

        var backlinks: [SubscribingTransitionVM<N>] {
            backlinksTransitions.compactMap { destinationVMs[TransitionKey($0, direction: .backward)] }
        }

        var tags: Set<String>
        var possibleTags: Set<String>

        @MainActor
        func toNodeState() -> NodeState {
            NodeState(
                data: data,
                favoriteLinks: favoriteLinks?.map { $0.eraseToAnyTransitionVM() },
                links: links.map { $0.eraseToAnyTransitionVM() },
                worseLinks: worseLinks?.map { $0.eraseToAnyTransitionVM() },
                backlinks: backlinks.map { $0.eraseToAnyTransitionVM() },
                tags: tags,
                possibleTags: possibleTags
            )
        }
    }

    private struct InvalidOperationForState: Error {
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
        case .loadedActive(_, let node, _):
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
        case .loadingActive(let .some(state), _):
            internalState = .loadedInactive(state: state)
        case .loadedActive(let state, _, let childNodeCancelSubjects):
            for (_, subject) in childNodeCancelSubjects {
                subject.send()
            }
            internalState = .loadedInactive(state: state)
        case .idle, .loadingInactive, .loadedInactive, .failed:
            // nothing to do, already in an okay state
            break
        }
        logInfo("\(nid) done")
    }

    struct DuplicateSubscription: Error {}

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
            logError(error)
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

    private struct UpdateRequest {
        let source: String
        let timestamp: Date = .now
    }

    private func updateStateLoop() async throws {
        guard case .loadingActive(_, let .some(node)) = internalState else { return }

        let channel = AsyncChannel<UpdateRequest>()

        try await withThrowingDiscardingTaskGroup { group in
            var lastUpdate = Date.now
            group.addTask {
                for await _ in node.metaPublisher.values {
                    await channel.send(UpdateRequest(source: "metaPublisher"))
                }
            }
            for await request in channel {
                if request.timestamp > lastUpdate {
                    logInfo("\(nid) updating state from \(request.source)")
                    lastUpdate = .now
                    try await updateState(channel, &group)
                } else {
                    logDebug("\(nid) skipping update because it is out of date")
                }
            }
        }
    }

    private func updateState(
        _ channel: AsyncChannel<UpdateRequest>,
        _ group: inout ThrowingDiscardingTaskGroup<any Error>
    ) async throws {
        guard case .loadingActive(_, let .some(node)) = internalState else { return }

        try Task.checkCancellation()

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

        logDebug("\(nid) done fetching data from node")
        try Task.checkCancellation()

        var favorites = [NodeTransition]()
        var normal = [NodeTransition]()
        var worse = [NodeTransition]()

        let favoritesSet: Set<NID>? = favoritesNode.map { Set($0.outgoing.map { $0.nid }) }
        let worseSet: Set<NID>? = worseNode.map { Set($0.outgoing.map { $0.nid }) }

        try Task.checkCancellation()
        logDebug("\(nid) creating children")

        for child in node.outgoing {
            if let favoritesSet, favoritesSet.contains(child.nid) {
                favorites.append(child)
            } else if let worseSet, worseSet.contains(child.nid) {
                worse.append(child)
            } else {
                normal.append(child)
            }
        }

        try Task.checkCancellation()
        logDebug("\(nid) about to create state")

        guard let (previousState, node, childNodeCancelSubjects) = internalState.validStateForUpdate else {
            // we need to rerun beginUpdateState because the state has drifted to an invalid one while doing IO
            return
        }

        func mkTransitionVMs(_ transitions: [NodeTransition], inDirection direction: Direction, isFavorite: Bool, isWorse: Bool) -> [SubscribingTransitionVM<N>] {
            transitions.map {
                SubscribingTransitionVM(parent: self, source: node, transition: $0, direction: direction, manager: self.manager, isFavorite: isFavorite, isWorse: isWorse)
            }
        }

        let favoriteLinks = favoritesSet != nil ? mkTransitionVMs(favorites, inDirection: .forward, isFavorite: true, isWorse: false) : nil
        let links = mkTransitionVMs(normal, inDirection: .forward, isFavorite: false, isWorse: false)
        let worseLinks = worseSet != nil ? mkTransitionVMs(worse, inDirection: .forward, isFavorite: false, isWorse: true) : nil
        let backlinks = mkTransitionVMs(node.incoming, inDirection: .backward, isFavorite: false, isWorse: false)

        let allDestinationVMs = (favoriteLinks ?? []) + links + (worseLinks ?? []) + backlinks
        var newDestinationVMs = allDestinationVMs
            .map { (TransitionKey(transition: $0.transition, destination: $0.destinationNid, direction: $0.direction), $0) }
            .to(Dictionary.init(uniqueKeysWithValues:))
        var newChildNodeCancelSubjects = childNodeCancelSubjects
        if let previousState {
            for (nodeTransitionKey, newVM) in newDestinationVMs
            where newVM.destinationNid != self.nid
            && !newChildNodeCancelSubjects.keys.contains(nodeTransitionKey.transition.nid) {
                assert(newChildNodeCancelSubjects[nodeTransitionKey.transition.nid] == nil)
                let cancelSubject = PassthroughSubject<(), Never>()
                newChildNodeCancelSubjects[nodeTransitionKey.transition.nid] = cancelSubject
                let publisher = newVM.updatedPublisher().prefix(untilOutputFrom: cancelSubject)
                let _ = group.addTaskUnlessCancelled {
                    logInfo("started task")
                    for await _ in publisher.values {
                        await channel.send(UpdateRequest(source: "\(nodeTransitionKey.transition.nid)"))
                    }
                    logInfo("publisher did finish so task can be cleaned up!")
                }
            }

            let removedDestinationNids = previousState
                .destinationVMs
                .filter { key, _ in !newDestinationVMs.keys.contains(key) }
                .map { key, value in key.transition.nid }
                .to(Set.init)
            for nid in removedDestinationNids {
                assert(newChildNodeCancelSubjects[nid] != nil)
                newChildNodeCancelSubjects[nid]?.send()
                newChildNodeCancelSubjects.removeValue(forKey: nid)
            }

            // prefer a TransitionVM that we previously created if it exists to avoid recreating everything
            // NOTE: this potentially introduces a bug where the isFavorite/isWorse value could get out of sync
            // because we keep the old value instead of the newly created value which might have been updated
            // this seems sufficiently unlikely that it's okay until it becomes a problem
            newDestinationVMs = previousState
                .destinationVMs
                .filter { key, _ in newDestinationVMs.keys.contains(key) }
                .map { key, value in (key, value) }
                .to(Dictionary.init(uniqueKeysWithValues:))
                .merging(newDestinationVMs) { oldVM, _ in oldVM }
        }

        // This currently doesn't work even though we're using it because we don't reload the NodeVM when the timestamps are loaded in
        // We need to update this so that NodeVM refreshes in response to updates in TransitionVM
        func timestampTransitionComparison(_ t1: NodeTransition, _ t2: NodeTransition) -> Bool {
            let t1Timestamp: Date
            if let vm = newDestinationVMs[TransitionKey(t1, direction: .forward)],
               case .loaded(.some(let timestamp)) = vm.timestamp {
                t1Timestamp = timestamp
            } else if let vm = newDestinationVMs[TransitionKey(t1, direction: .backward)],
                      case .loaded(.some(let timestamp)) = vm.timestamp {
                t1Timestamp = timestamp
            } else {
                t1Timestamp = .distantPast
            }

            let t2Timestamp: Date
            if let vm = newDestinationVMs[TransitionKey(t2, direction: .forward)],
               case .loaded(.some(let timestamp)) = vm.timestamp {
                t2Timestamp = timestamp
            } else if let vm = newDestinationVMs[TransitionKey(t2, direction: .backward)],
                      case .loaded(.some(let timestamp)) = vm.timestamp {
                t2Timestamp = timestamp
            } else {
                t2Timestamp = .distantPast
            }

            return t1Timestamp > t2Timestamp || t1Timestamp == t2Timestamp && t1.transition < t2.transition
        }

        let favoriteLinksTransitions = favoriteLinks?
            .map { NodeTransition(transition: $0.transition, nid: $0.destinationNid) }
            .to(Array.init)
            .sorted(by: timestampTransitionComparison)
        let linksTransitions = links
            .map { NodeTransition(transition: $0.transition, nid: $0.destinationNid) }
            .to(Array.init)
            .sorted(by: timestampTransitionComparison)
        let worseLinksTransitions = worseLinks?
            .map { NodeTransition(transition: $0.transition, nid: $0.destinationNid) }
            .to(Array.init)
            .sorted(by: timestampTransitionComparison)
        let backlinksTransitions = backlinks
            .map { NodeTransition(transition: $0.transition, nid: $0.destinationNid) }
            .to(Array.init)
            .sorted(by: timestampTransitionComparison)

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

        internalState = .loadedActive(state: state, node: node, childNodeCancelSubjects: newChildNodeCancelSubjects)

        logInfo("\(nid) updated state")

        for vm in newDestinationVMs {
            if case .idle = vm.value.timestamp {
                try await vm.value.fetchTimestamp()
            }
        }
    }

    func reload() async {
        internalState = .loadingInactive
        // if subscribed, `updateStateLoop` will exit because of the unexpected state and `beginUpdateState` will rerun
    }
}
