//
//  NodeVM.swift
//  view
//
//  Created by Devin Lehmacher on 3/27/23.
//

import Foundation

@MainActor
class NodeVM<N: Node>: ObservableObject {
    let nid: NID

    @Published var state: Loading<State> = .idle

    struct State {
        fileprivate let node: N
        fileprivate let manager: GraphManager<N>

        var data: Data?

        var favoriteLinks: [TransitionVM<N>]?
        var links: [TransitionVM<N>]
        var worseLinks: [TransitionVM<N>]?
        var backlinks: [TransitionVM<N>]

        var tags: Set<String>
        var possibleTags: Set<String>

        func set(tags: Set<String>) async {
            await node.set(tags: tags)
        }

        func forceRemove() async {
            await manager.forceRemove(node: node)
        }
    }

    private let manager: GraphManager<N>

    init(for nid: NID, in graph: GraphManager<N>) {
        self.nid = nid
        self.manager = graph
    }

    func load() async {
        guard case .idle = state else {
            return
        }
        
        state = .loading
        let node: N
        let favoritesNode: N?
        let worseNode: N?
        let data: DataDocument<N>?
        let tags: Set<String>
        let tagOptions: Set<String>
        do {
            node = try await manager[nid].unwrapped("node \(nid) doesn't exist")
            async let favoritesAsync = node.favorites
            async let worseAsync = node.worse
            async let dataAsync = node.data
            async let tagsAsync = node.tags
            async let possibleTagsAsync = manager.tags?.tagOptions
            favoritesNode = await favoritesAsync
            worseNode = await worseAsync
            data = await dataAsync
            tags = await tagsAsync
            tagOptions = await possibleTagsAsync ?? Set()
        } catch {
            logError(error.localizedDescription)
            state = .failed(error)
            return
        }

        var favorites = [NodeTransition]()
        var normal = [NodeTransition]()
        var worse = [NodeTransition]()

        let favoritesSet: Set<NID>? = favoritesNode.map { Set($0.outgoing.map { $0.nid }) }
        let worseSet: Set<NID>? = worseNode.map { Set($0.outgoing.map { $0.nid }) }

        for child in node.outgoing {
            if let favoritesSet, favoritesSet.contains(child.nid) {
                favorites.append(child)
            } else if let worseSet, worseSet.contains(child.nid) {
                worse.append(child)
            } else {
                normal.append(child)
            }
        }

        func mkTransitionVMs(_ transitions: [NodeTransition], inDirection direction: Direction, isFavorite: Bool, isWorse: Bool) -> [TransitionVM<N>] {
            transitions.sorted().map {
                TransitionVM(parent: self, source: node, transition: $0, direction: direction, manager: self.manager, isFavorite: isFavorite, isWorse: isWorse)
            }
        }

        state = .loaded(State(
            node: node,
            manager: manager,
            data: data?.data,
            favoriteLinks: favoritesSet != nil ? mkTransitionVMs(favorites, inDirection: .forward, isFavorite: true, isWorse: false) : nil,
            links: mkTransitionVMs(normal, inDirection: .forward, isFavorite: false, isWorse: false),
            worseLinks: worseSet != nil ? mkTransitionVMs(worse, inDirection: .forward, isFavorite: false, isWorse: true) : nil,
            backlinks: mkTransitionVMs(node.incoming, inDirection: .backward, isFavorite: false, isWorse: false),
            tags: tags,
            possibleTags: tagOptions
        ))
    }

    func toggleFavorite(child _: NID) async {
//        if case .loaded(let node) = node {
//            await node.toggleFavorite(child: child)
//            var isFavoriteNow = await node.isFavorite(child: child)
//            if case .loaded(let state) = state {
//                var state = state
//                if isFavoriteNow {
//                    state.favoriteLinks?.append(contentsOf: state.links.filter { $0.destinationNid == child})
//                    state.links.
//                } else {
//                    state.links.append
//                    state.favoriteLinks?.removeAll { $0.destinationNid == child
//                    }
//                }
//            }
//        } else {
//            logInfo("skipping toggleFavorite because NodeVM not loaded")
//        }
    }

    func toggleWorse(child _: NID) async {
    }
}
