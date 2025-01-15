//
//  MockNodeVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/22/24.
//

import Foundation

class MockNodeVM: NodeVM {
    let nid: NID
    var state: Loading<NodeState>

    init(nid: NID, state: Loading<NodeState> = .loading) {
        self.nid = nid
        self.state = state
    }

    convenience init(
        nid: NID,
        data: Data? = nil,
        favoriteLinks: [AnyTransitionVM]? = nil,
        links: [AnyTransitionVM] = [],
        worseLinks: [AnyTransitionVM]? = nil,
        backlinks: [AnyTransitionVM] = [],
        tags: Set<String> = Set(),
        possibleTags: Set<String> = Set()
    ) {
        self.init(
            nid: nid,
            state: .loaded(
                NodeState(
                    data: data,
                    favoriteLinks: favoriteLinks,
                    links: links,
                    worseLinks: worseLinks,
                    backlinks: backlinks,
                    tags: tags,
                    possibleTags: possibleTags
                )
            )
        )
    }

    func subscribe() async {}
    func reload() async {}
    func set(tags: Set<String>) throws {}
    func forceRemove() throws {}
    func toggleFavorite(child: NID) throws {}
    func toggleWorse(child: NID) throws {}
}
