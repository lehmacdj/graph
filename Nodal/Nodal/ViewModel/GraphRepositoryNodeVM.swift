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
        guard !isSubscribed else {
            // guaranteeing that there is only one process executing subscribe makes it easier to reason about
            logWarn("duplicate subscribe call")
            return
        }
        isSubscribed = true
        defer {
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
            let updatesSequence = await graphRepository.updates(computeValue: nid.computeNodeForVM)
            for try await value in updatesSequence {
                state = .loaded(NodeState(
                    data: value.data,
                    favoriteLinks: value.favoriteLinks
                        .sorted(by: timestampTransitionComparison(_:_:))
                        .map { getTransitionVM(key: $0.0, timestamp: $0.1, configuredForSection: .favorites) },
                    links: value.otherLinks
                        .sorted(by: timestampTransitionComparison(_:_:))
                        .map { getTransitionVM(key: $0.0, timestamp: $0.1, configuredForSection: .other) },
                    worseLinks: value.worseLinks
                        .sorted(by: timestampTransitionComparison(_:_:))
                        .map { getTransitionVM(key: $0.0, timestamp: $0.1, configuredForSection: .worse) },
                    backlinks: value.backlinks
                        .sorted(by: timestampTransitionComparison(_:_:))
                        .map { getTransitionVM(key: $0.0, timestamp: $0.1, configuredForSection: .backlink) },
                    tags: value.tags,
                    possibleTags: value.possibleTags
                ))
                endTransitionVMsGeneration()
            }
        } catch {
            logError(error)
            state = .failed(error)
        }
    }

    private var transitionVMsPrevious = [TransitionKey: GraphRepositoryTransitionVM]()
    private var transitionVMs = [TransitionKey: GraphRepositoryTransitionVM]()

    enum NodeSection {
        case favorites
        case worse
        case other
        case backlink

        var direction: Direction {
            switch self {
            case .favorites, .worse, .other: .forward
            case .backlink: .backward
            }
        }
    }

    /// Mark that we are done creating transition VMs for an update cycle
    private func endTransitionVMsGeneration() {
        transitionVMsPrevious = transitionVMs
        transitionVMs = [:]
    }

    private func getTransitionVM(
        key: NodeTransition,
        timestamp: Loading<Date?>,
        configuredForSection section: NodeSection
    ) -> AnyTransitionVM {
        // TODO: try axing the caching to see if GraphRepository is doing a sufficient job in caching things that are already loaded itself
        if let vm = transitionVMsPrevious[TransitionKey(key, direction: section.direction)] {
            vm.configureForSection(section)
            vm.timestamp = timestamp
            return vm.eraseToAnyTransitionVM()
        } else {
            let vm = GraphRepositoryTransitionVM(
                graphRepository: graphRepository,
                sourceNid: nid,
                transition: key,
                timestamp: timestamp,
                configuredForSection: section
            )
            transitionVMs[TransitionKey(key, direction: section.direction)] = vm
            return vm.eraseToAnyTransitionVM()
        }
    }

    func reload() async {
        // not sure if reload is actually necessary if I manage to make the auto-refresh reliable enough
        // maybe I can let this reload even failed nodes?
    }

    // MARK: these have been no-op-ed for now because we don't support modifying the graph yet

    func set(tags: Set<String>) async throws {}

    func forceRemove() async throws {}

    func toggleFavorite(child _: NID) async throws {}

    func toggleWorse(child _: NID) async throws {}
}

fileprivate struct NodeStateAugmentation {
    let data: Data?
    let favoriteLinks: [(NodeTransition, Loading<Date?>)]
    let worseLinks: [(NodeTransition, Loading<Date?>)]
    let backlinks: [(NodeTransition, Loading<Date?>)]
    let otherLinks: [(NodeTransition, Loading<Date?>)]
    let tags: Set<String>
    let possibleTags: Set<String>
}

private extension NID {
    func computeNodeForVM(_ dependencyManager: DependencyManager) throws(FetchDependencyError) -> NodeValue<NodeStateAugmentation> {
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

        let allTags = try dependencyManager.fetch(nid: NID.tags, dataNeed: .dataNotNeeded)
            .outgoing
            .keys
            .to(Set.init)
        let tags = allTags.filter { node.outgoing[$0] != nil }

        let data = try dependencyManager
            .fetch(nid: self, dataNeed: .needDataEvenIfRemote)
            .data

        return node.withAugmentation(
            NodeStateAugmentation(
                data: data,
                favoriteLinks: links.filter { favoritesNids.contains($0.0.nid) },
                worseLinks: links.filter { worseNids.contains($0.0.nid) },
                backlinks: backlinks,
                otherLinks: links.filter { !favoritesNids.contains($0.0.nid) && !worseNids.contains($0.0.nid) },
                tags: tags,
                possibleTags: allTags
            )
        )
    }
}

func timestampTransitionComparison(_ tuple1: (NodeTransition, Loading<Date?>), _ tuple2: (NodeTransition, Loading<Date?>)) -> Bool {
    let (t1, t1LoadingTimestamp) = tuple1
    let t1Timestamp: Date
    if case .loaded(.some(let timestamp)) = t1LoadingTimestamp {
        t1Timestamp = timestamp
    } else {
        t1Timestamp = .distantPast
    }

    let (t2, t2LoadingTimestamp) = tuple2
    let t2Timestamp: Date
    if case .loaded(.some(let timestamp)) = t2LoadingTimestamp {
        t2Timestamp = timestamp
    } else {
        t2Timestamp = .distantPast
    }

    return t1.transition < t2.transition || t1.transition == t2.transition && t1Timestamp > t2Timestamp
}

extension GraphRepositoryNodeVM: CustomStringConvertible {
    var description: String {
        "\(type(of: self)):\(self.nid)"
    }
}

extension GraphRepositoryNodeVM: LogContextProviding {
    var logContext: [String] { [description] }
}
