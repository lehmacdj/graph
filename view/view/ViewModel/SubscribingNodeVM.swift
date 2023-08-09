//
//  SubscribingNodeVM.swift
//  view
//
//  Created by Devin Lehmacher on 8/6/23.
//

import Foundation
import Observation
import Combine

@MainActor class SubscribingNodeVM<N: Node>: ObservableObject, NodeVM {
    let nid: NID

    @Published var state: Loading<any NodeState<N>> = .idle

    struct State: NodeState {
        fileprivate let node: N
        fileprivate let manager: GraphManager<N>

        var data: Data?

        var favoriteLinks: [AnyTransitionVM<N>]?
        var links: [AnyTransitionVM<N>]
        var worseLinks: [AnyTransitionVM<N>]?
        var backlinks: [AnyTransitionVM<N>]

        var tags: Set<String>
        var possibleTags: Set<String>

        func set(tags: Set<String>) async {
            await node.set(tags: tags)
        }

        func forceRemove() async {
            await manager.forceRemove(node: node)
        }

        func toggleFavorite(child _: NID) async {
        }

        func toggleWorse(child _: NID) async {
        }
    }

    private let manager: GraphManager<N>

    init(for nid: NID, in graph: GraphManager<N>) {
        self.nid = nid
        self.manager = graph
    }

    private var task: Task<Void, Never>?

    func load() async {
        guard case .idle = state else {
            logDebug("skipping initializing of state because already loading")
            return
        }

        logDebug("starting to initialize state")

        state = .loading
        let node: N
        do {
            node = try await manager[nid]
        } catch {
            logError(error.localizedDescription)
            state = .failed(error)
            return
        }

        logDebug("fetched initial node state")

        let semaphore = Semaphor(initialCount: 0)
        let task = Task.detached { [weak self, weak semaphore] in
            guard let self else { return }

            for await _ in node.metaPublisher.values {
                // optimization opportunity: avoid using async for accessing some node properties, some of this data shouldn't require async, it's just a pure computation to get it from the node's metadata
                async let favoritesAsync = node.favorites
                async let worseAsync = node.worse
                async let dataAsync = node.data
                async let tagsAsync = node.tags
                async let possibleTagsAsync = manager.tags?.tagOptions
                let favoritesNode = await favoritesAsync
                let worseNode = await worseAsync
                let data = await dataAsync
                let tags = await tagsAsync
                let tagOptions = await possibleTagsAsync ?? Set()

                logDebug("done fetching data from node")

                var favorites = [NodeTransition]()
                var normal = [NodeTransition]()
                var worse = [NodeTransition]()

                let favoritesSet: Set<NID>? = favoritesNode.map { Set($0.outgoing.map { $0.nid }) }
                let worseSet: Set<NID>? = worseNode.map { Set($0.outgoing.map { $0.nid }) }

                logDebug("creating children")

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

                logDebug("about to create state")

                let state: Loading<any NodeState<N>> = await .loaded(State(
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
                Task { @MainActor in
                    self.state = state
                    logDebug("updated state")
                }

                logDebug("finished fetching data")

                if await self.task == nil {
                    await semaphore?.signal()
                }
            }
        }
        await semaphore.wait()
        self.task = task
    }
}
