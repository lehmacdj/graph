//
//  GraphRepositoryNodeVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 12/15/24.
//

import Foundation
import AsyncAlgorithms

@Observable
final class GraphRepositoryNodeVM: NodeVM {
    private let graphRepository: GraphRepository
    let nid: NID

    init(nid: NID, graphRepository: GraphRepository) {
        self.nid = nid
        self.graphRepository = graphRepository
    }

    var state: Loading<NodeState> = .idle

    var isSubscribed = false

    func subscribe() async {
        await withPushLogContext("NodeVM(\(nid)).subscribe", operation: _subscribe)
    }

    func _subscribe() async {
        guard !isSubscribed else {
            // guaranteeing that there is only one process executing subscribe makes it easier to reason about
            logWarn("duplicate subscribe call")
            return
        }
        logDebug("beginning subscription")
        isSubscribed = true
        defer {
            logDebug("ending subscription")
            isSubscribed = false
            switch state {
            case .loading: state = .idle
            default: break
            }
        }
        switch state {
        case .idle: state = .loading
        case .failed: return
        case .loaded, .loading: break
        }

        do {
            let updatesSequence = await graphRepository.updates(computeValue: nid.computeNodeForVM(sortOrder: sortOrder))
            logInfo("starting to listen to updates")
            for try await value in updatesSequence {
                state = .loaded(NodeState(
                    data: value.data,
                    dataURL: value.data.map { _ in graphRepository.getHypotheticalDataPath(for: nid) },
                    favoriteLinks: value.favoriteLinks
                        .sorted(by: sortOrder.weirdDataComparisonFunction)
                        .map { getTransitionVM(transition: $0.0, timestamp: $0.1, configuredForSection: .favorites) },
                    links: value.otherLinks
                        .sorted(by: sortOrder.weirdDataComparisonFunction)
                        .map { getTransitionVM(transition: $0.0, timestamp: $0.1, configuredForSection: .other) },
                    worseLinks: value.worseLinks
                        .sorted(by: sortOrder.weirdDataComparisonFunction)
                        .map { getTransitionVM(transition: $0.0, timestamp: $0.1, configuredForSection: .worse) },
                    backlinks: value.backlinks
                        .sorted(by: sortOrder.weirdDataComparisonFunction)
                        .map { getTransitionVM(transition: $0.0, timestamp: $0.1, configuredForSection: .backlink) },
                    tags: value.tags
                ))
                endTransitionVMsGeneration()
            }
            logDebug("exited update loop (Task.isCancelled=\(Task.isCancelled))")
        } catch {
            logError(error)
            state = .failed(error)
        }
    }

    private var transitionVMsPrevious = [TransitionKey: GraphRepositoryTransitionVM]()
    private var transitionVMs = [TransitionKey: GraphRepositoryTransitionVM]()

    var sortOrder: NodeSortOrder = .transitionThenTimestamp(timestampOrder: .newerFirst) {
        didSet {
            guard let currentState = state.loaded else { return }
            state = .loaded(NodeState(
                data: currentState.data,
                dataURL: currentState.data.map { _ in graphRepository.getHypotheticalDataPath(for: nid) },
                favoriteLinks: currentState.favoriteLinks?
                    .sorted(by: sortOrder.transitionVMComparisonFunction),
                links: currentState.links
                    .sorted(by: sortOrder.transitionVMComparisonFunction),
                worseLinks: currentState.worseLinks?
                    .sorted(by: sortOrder.transitionVMComparisonFunction),
                backlinks: currentState.backlinks
                    .sorted(by: sortOrder.transitionVMComparisonFunction),
                tags: currentState.tags
            ))
        }
    }

    /// Mark that we are done creating transition VMs for an update cycle
    private func endTransitionVMsGeneration() {
        transitionVMsPrevious = transitionVMs
        transitionVMs = [:]
    }

    private func getTransitionVM(
        transition: NodeTransition,
        timestamp: Loading<Date?>,
        configuredForSection section: NodeSection
    ) -> AnyTransitionVM {
        let key = TransitionKey(transition, direction: section.direction)
        if let vm = transitionVMsPrevious[key] {
            vm.configureForSection(section)
            vm.timestamp = timestamp
            transitionVMs[key] = vm
            return vm.eraseToAnyTransitionVM()
        } else {
            let vm = GraphRepositoryTransitionVM(
                graphRepository: graphRepository,
                sourceNid: nid,
                transition: transition,
                timestamp: timestamp,
                configuredForSection: section
            )
            transitionVMs[key] = vm
            return vm.eraseToAnyTransitionVM()
        }
    }

    func reload() async {
        // not sure if reload is actually necessary if I manage to make the auto-refresh reliable enough
        // maybe I can let this reload even failed nodes?
    }

    // MARK: these have been no-op-ed for now because we don't support modifying the graph yet

    func set(tags: Set<String>) throws {}

    func forceRemove() throws {}

    func toggleFavorite(child _: NID) throws {}

    func toggleWorse(child _: NID) throws {}
}

fileprivate struct NodeStateAugmentation: Sendable {
    let data: Data?
    let favoriteLinks: [(NodeTransition, Loading<Date?>)]
    let worseLinks: [(NodeTransition, Loading<Date?>)]
    let backlinks: [(NodeTransition, Loading<Date?>)]
    let otherLinks: [(NodeTransition, Loading<Date?>)]
    let tags: [String]
}

private extension NID {
    func computeNodeForVM(sortOrder: NodeSortOrder) -> @Sendable (_ dependencyManager: DependencyManager) throws -> Node<NodeStateAugmentation> {
        { dependencyManager in
            try self._computeNodeForVM(sortOrder: sortOrder, dependencyManager)
        }
    }

    private func _computeNodeForVM(sortOrder: NodeSortOrder, _ dependencyManager: DependencyManager) throws(FetchDependencyError) -> Node<NodeStateAugmentation> {
        // avoid fetching data at first even though we unconditionally render it to start fetching metadata sooner
        let node = try dependencyManager.fetch(nid: self, dataNeed: .dataNotNeeded)

        let favoritesNids = (node.outgoing["favorites"] ?? [])
            // probably letting node render even if these aren't up to date/resolved is best
            // if not, we could throw later after discovering as many dependencies as we can
            .compactMap { try? dependencyManager.fetch(nid: $0, dataNeed: .dataNotNeeded) }
            .flatMap { $0.outgoing.values }
            .unioned()
        let worseNids = (node.outgoing["worse"] ?? [])
            // probably letting node render even if these aren't up to date/resolved is best
            // if not, we could throw later after discovering as many dependencies as we can
            .compactMap { try? dependencyManager.fetch(nid: $0, dataNeed: .dataNotNeeded) }
            .flatMap { $0.outgoing.values }
            .unioned()
        let links = node
            .outgoingTransitions
            .map { transition in
                let timestamped = try? transition.nid.computeNodeWithTimestamp(dependencyManager: dependencyManager)
                return (transition, Loading.loading(timestamped.map { $0.timestamp }))
            }
        let backlinks = node
            .incomingTransitions
            .map { transition in
                let timestamped = try? transition.nid.computeNodeWithTimestamp(dependencyManager: dependencyManager)
                return (transition, Loading.loading(timestamped.map { $0.timestamp }))
            }


        let tags = try self.computeNodeWithTags(dependencyManager: dependencyManager)
            .tags
            .sorted()
        let data = try dependencyManager
            .fetch(nid: self, dataNeed: .needDataEvenIfRemote)
            .data

        // make sure that we can uniquely sort based on the data we have, otherwise transitions jump around wildly
        let favoriteLinks = links.filter { favoritesNids.contains($0.0.nid) }
        let worseLinks = links.filter { worseNids.contains($0.0.nid) }
        let otherLinks = links.filter { !favoritesNids.contains($0.0.nid) && !worseNids.contains($0.0.nid) }
        guard sortOrder.isSufficientForCompleteSort(links: otherLinks)
                && sortOrder.isSufficientForCompleteSort(links: favoriteLinks)
                && sortOrder.isSufficientForCompleteSort(links: worseLinks)
                && sortOrder.isSufficientForCompleteSort(links: backlinks)
        else {
            throw .missingDependencies
        }

        return node.withAugmentation(
            NodeStateAugmentation(
                data: data,
                favoriteLinks: favoriteLinks,
                worseLinks: worseLinks,
                backlinks: backlinks,
                otherLinks: otherLinks,
                tags: tags
            )
        )
    }
}

private extension NodeSortOrder {
    var weirdDataComparisonFunction: ( (NodeTransition, Loading<Date?>), (NodeTransition, Loading<Date?>) ) -> Bool {
        { tuple1, tuple2 in
            let (t1, t1LoadingTimestamp) = tuple1
            let t1Timestamp = t1LoadingTimestamp.loaded.flatMap { $0 } ?? .distantPast
            let (t2, t2LoadingTimestamp) = tuple2
            let t2Timestamp: Date = t2LoadingTimestamp.loaded.flatMap { $0 } ?? .distantPast
            let sortFunc: (Date, Date, String, String) -> Bool = switch self {
            case .timestampThenTransition(timestampOrder: .newerFirst):
                timestampNewerFirstThenTransitionOrdering(t1Timestamp:t2Timestamp:t1Transition:t2Transition:)
            case .timestampThenTransition(timestampOrder: .olderFirst):
                timestampOlderFirstThenTransitionOrdering(t1Timestamp:t2Timestamp:t1Transition:t2Transition:)
            case .transitionThenTimestamp(timestampOrder: .newerFirst):
                transitionThenTimestampNewerFirstOrdering(t1Timestamp:t2Timestamp:t1Transition:t2Transition:)
            case .transitionThenTimestamp(timestampOrder: .olderFirst):
                transitionThenTimestampOlderFirstOrdering(t1Timestamp:t2Timestamp:t1Transition:t2Transition:)
            }
            return sortFunc(t1Timestamp, t2Timestamp, t1.transition, t2.transition)
        }
    }

    var comparisonFunction: (Date, Date, String, String) -> Bool {
        switch self {
        case .timestampThenTransition(timestampOrder: .newerFirst):
            timestampNewerFirstThenTransitionOrdering(t1Timestamp:t2Timestamp:t1Transition:t2Transition:)
        case .timestampThenTransition(timestampOrder: .olderFirst):
            timestampOlderFirstThenTransitionOrdering(t1Timestamp:t2Timestamp:t1Transition:t2Transition:)
        case .transitionThenTimestamp(timestampOrder: .newerFirst):
            transitionThenTimestampNewerFirstOrdering(t1Timestamp:t2Timestamp:t1Transition:t2Transition:)
        case .transitionThenTimestamp(timestampOrder: .olderFirst):
            transitionThenTimestampOlderFirstOrdering(t1Timestamp:t2Timestamp:t1Transition:t2Transition:)
        }
    }

    var transitionVMComparisonFunction: @MainActor (AnyTransitionVM, AnyTransitionVM) -> Bool {
        { vm1, vm2 in
            let t1Timestamp = vm1.timestamp.loaded.flatMap { $0 } ?? .distantPast
            let t2Timestamp = vm2.timestamp.loaded.flatMap { $0 } ?? .distantPast
            return comparisonFunction(t1Timestamp, t2Timestamp, vm1.transition, vm2.transition)
        }
    }

    func isSufficientForCompleteSort(links: [(NodeTransition, Loading<Date?>)]) -> Bool {
        let uniqueTransitions = links
            .map(\.0.transition)
            .frequencies()
            .filter { $0.value == 1 }
            .keys
        return switch self {
        case .timestampThenTransition:
            true
        case .transitionThenTimestamp:
            links.allSatisfy { link in uniqueTransitions.contains(link.0.transition) || link.1.isLoaded }
        }
    }
}

private func timestampNewerFirstThenTransitionOrdering(
    t1Timestamp: Date,
    t2Timestamp: Date,
    t1Transition: String,
    t2Transition: String
) -> Bool {
    return t1Timestamp > t2Timestamp || t1Timestamp == t2Timestamp && t1Transition < t2Transition
}

private func transitionThenTimestampNewerFirstOrdering(
    t1Timestamp: Date,
    t2Timestamp: Date,
    t1Transition: String,
    t2Transition: String
) -> Bool {
    return t1Transition < t2Transition || t1Transition == t2Transition && t1Timestamp > t2Timestamp
}

private func timestampOlderFirstThenTransitionOrdering(
    t1Timestamp: Date,
    t2Timestamp: Date,
    t1Transition: String,
    t2Transition: String
) -> Bool {
    return t1Timestamp < t2Timestamp || t1Timestamp == t2Timestamp && t1Transition < t2Transition
}

private func transitionThenTimestampOlderFirstOrdering(
    t1Timestamp: Date,
    t2Timestamp: Date,
    t1Transition: String,
    t2Transition: String
) -> Bool {
    return t1Transition < t2Transition || t1Transition == t2Transition && t1Timestamp < t2Timestamp
}
