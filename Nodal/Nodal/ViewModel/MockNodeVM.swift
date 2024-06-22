//
//  MockNodeVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/22/24.
//

import Foundation

struct MockNodeState: NodeState {
    var data: Data?
    var favoriteLinks: [AnyTransitionVM]?
    var links: [AnyTransitionVM]
    var worseLinks: [AnyTransitionVM]?
    var backlinks: [AnyTransitionVM]
    var tags: Set<String>
    var possibleTags: Set<String>
}

class MockNodeVM: NodeVM {
    let nid: NID
    var state: Loading<any NodeState>

    init(nid: NID, state: Loading<any NodeState> = .loading) {
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
                MockNodeState(
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
    func set(tags: Set<String>) async throws {}
    func forceRemove() async throws {}
    func toggleFavorite(child: NID) async throws {}
    func toggleWorse(child: NID) async throws {}
}
