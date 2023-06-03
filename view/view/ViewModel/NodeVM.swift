//
//  NodeVM.swift
//  view
//
//  Created by Devin Lehmacher on 3/27/23.
//

import Foundation

@MainActor
class NodeVM: ObservableObject {
    let nid: NID

    @Published var node: Loading<Node> = .idle
    @Published var state: Loading<State> = .idle

    struct State {
        fileprivate let node: Node
        fileprivate let manager: GraphManager

        let data: Data?

        let favoriteLinks: [TransitionVM]?
        let links: [TransitionVM]
        let worseLinks: [TransitionVM]?
        let backlinks: [TransitionVM]

        let tags: Set<String>
        let possibleTags: Set<String>

        func set(tags: Set<String>) async {
            await node.set(tags: tags)
        }

        func forceRemove() async {
            await manager.forceRemove(node: node)
        }
    }

    private let manager: GraphManager

    init(for nid: NID, in graph: GraphManager) {
        self.nid = nid
        self.manager = graph
    }

    func load() async {
        state = .loading
        let node: Node
        let favoritesNode: Node?
        let worseNode: Node?
        let data: Data?
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
            data = try await dataAsync
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

        let favoritesSet: Set<NodeTransition>? = favoritesNode.map { Set($0.outgoing) }
        let worseSet: Set<NodeTransition>? = worseNode.map { Set($0.outgoing) }

        for child in node.outgoing {
            if let favoritesSet, favoritesSet.contains(child) {
                favorites.append(child)
            } else if let worseSet, worseSet.contains(child) {
                worse.append(child)
            } else {
                normal.append(child)
            }
        }

        func mkTransitionVMs(_ transitions: [NodeTransition], inDirection direction: Direction, isFavorite: Bool, isWorse: Bool) -> [TransitionVM] {
            transitions.sorted().map {
                TransitionVM(source: node, transition: $0, direction: direction, manager: self.manager, isFavorite: isFavorite, isWorse: isWorse)
            }
        }

        state = .loaded(State(
            node: node,
            manager: manager,
            data: data,
            favoriteLinks: favoritesSet != nil ? mkTransitionVMs(favorites, inDirection: .forward, isFavorite: true, isWorse: false) : nil,
            links: mkTransitionVMs(normal, inDirection: .forward, isFavorite: false, isWorse: false),
            worseLinks: worseSet != nil ? mkTransitionVMs(worse, inDirection: .forward, isFavorite: false, isWorse: true) : nil,
            backlinks: mkTransitionVMs(node.incoming, inDirection: .backward, isFavorite: false, isWorse: false),
            tags: tags,
            possibleTags: tagOptions
        ))
    }
}
