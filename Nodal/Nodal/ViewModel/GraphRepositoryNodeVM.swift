//
//  GraphRepositoryNodeVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 12/15/24.
//

import Foundation

fileprivate struct NodeStateAugmentation {
    let data: Data?
    let favoriteLinks: [(NodeTransition, Date?)]
    let worseLinks: [(NodeTransition, Date?)]
    let backlinks: [(NodeTransition, Date?)]
    let otherLinks: [(NodeTransition, Date?)]
    let tags: Set<String>
    let possibleTags: Set<String>
}

//extension NodeStateAugmentation {
//    static func computeDependentNodes(forAugmenting id: NID, from node: NodeValue<Void>) -> Set<NID> {
//        guard id == node.id else {
//            return []
//        }
//        let favorites = node.outgoing["favorites"] ?? []
//        let worse = node.outgoing["worse"] ?? []
//        return [[NID.tags], favorites, worse].unioned()
//    }
//
//    static func computeDataNeed(forAugmenting id: NID, from node: NodeValue<Void>) -> AugmentationDataNeed {
//        if node.id == id {
//            .needDataEvenIfRemote
//        } else {
//            .dataNotNeeded
//        }
//    }
//
//    static func computeAugmentation(for id: NID, dependencies: [NID : NodeValue<AugmentationDataValue>]) -> NodeStateAugmentation {
//        let node = dependencies[id]!
//        let favorites = (node.outgoing["favorites"] ?? [])
//            .flatMap { dependencies[$0]!.outgoing.values }
//            .unioned()
//        let worse = (node.outgoing["worse"] ?? [])
//            .flatMap { dependencies[$0]!.outgoing.values }
//            .unioned()
//        let links = node.outgoingTransitions
//        let tags = dependencies[NID.tags]!.outgoing
//            .filter { (key, value) in value.contains(id) }
//            .keys
//            .to(Set.init)
//        return NodeStateAugmentation(
//            data: nil, // TODO: update once I implement data support
//            favoriteLinks: links.filter { favorites.contains($0.nid) },
//            worseLinks: links.filter { worse.contains($0.nid) },
//            backlinks: node.incomingTransitions,
//            otherLinks: links.filter { !favorites.contains($0.nid) && !worse.contains($0.nid) },
//            tags: tags,
//            possibleTags: dependencies[NID.tags]!.outgoing.keys.to(Set.init)
//        )
//    }
//}
//
//private extension NID {
//    func computeNodeState(_ fetchDependency: FetchDependencyClosure) throws(FetchDependencyError) -> NodeValue<NodeStateAugmentation> {
//        let node = fetchDependency(self, .dataNotNeeded)
//        let favorites = (node.outgoing["favorites"] ?? [])
//            .flatMap { dependencies[$0]!.outgoing.values }
//            .unioned()
//        let worse = (node.outgoing["worse"] ?? [])
//            .flatMap { dependencies[$0]!.outgoing.values }
//            .unioned()
//        let links = node.outgoingTransitions
//        let tags = dependencies[NID.tags]!.outgoing
//            .filter { (key, value) in value.contains(id) }
//            .keys
//            .to(Set.init)
//        return NodeStateAugmentation(
//            data: nil, // TODO: update once I implement data support
//            favoriteLinks: links.filter { favorites.contains($0.nid) },
//            worseLinks: links.filter { worse.contains($0.nid) },
//            backlinks: node.incomingTransitions,
//            otherLinks: links.filter { !favorites.contains($0.nid) && !worse.contains($0.nid) },
//            tags: tags,
//            possibleTags: dependencies[NID.tags]!.outgoing.keys.to(Set.init)
//        )
//    }
//}

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
            for try await value in await graphRepository.updates(computeValue: nid.computeNodeState) { value in
                state = .loaded(NodeState(
                    data: value.augmentation.data,
                    favoriteLinks: value.augmentation.favoriteLinks
                        .map { getTransitionVM(key: $0, configuredForSection: .favorites) },
                    links: value.augmentation.otherLinks
                        .map { getTransitionVM(key: $0, configuredForSection: .other) },
                    worseLinks: value.augmentation.worseLinks
                        .map { getTransitionVM(key: $0, configuredForSection: .worse) },
                    backlinks: value.augmentation.backlinks
                        .map { getTransitionVM(key: $0, configuredForSection: .backlink) },
                    tags: value.augmentation.tags,
                    possibleTags: value.augmentation.possibleTags
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
        configuredForSection section: NodeSection
    ) -> AnyTransitionVM {
        // TODO: try axing the caching to see if GraphRepository is doing a sufficient job in caching things that are already loaded itself
        if let vm = transitionVMsPrevious[TransitionKey(key, direction: section.direction)] {
            vm.configureForSection(section)
            return vm.eraseToAnyTransitionVM()
        } else {
            let vm = GraphRepositoryTransitionVM(
                graphRepository: graphRepository,
                transition: key,
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
