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
        favoriteLinks: [TransitionTemplate]? = nil,
        links: [TransitionTemplate] = [],
        worseLinks: [TransitionTemplate]? = nil,
        backlinks: [TransitionTemplate] = [],
        tags: [String] = []
    ) {
        self.init(
            nid: nid,
            state: .loaded(
                NodeState(
                    data: data,
                    favoriteLinks: favoriteLinks.map { $0.map(\.asFavorites) },
                    links: links.map(\.asOther),
                    worseLinks: worseLinks.map { $0.map(\.asWorse) },
                    backlinks: backlinks.map(\.asBacklink),
                    tags: tags
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

extension MockNodeVM {
    struct TransitionTemplate {
        let transition: String
        let thumbnail: Loading<ThumbnailValue>
        let timestamp: Loading<Date?>
        let destination: AnyNodeVM?

        init(
            transition: String = "",
            thumbnail: Loading<ThumbnailValue> = .loaded(.noThumbnail),
            timestamp: Loading<Date?> = .loaded(nil),
            destination: AnyNodeVM? = nil
        ) {
            self.transition = transition
            self.thumbnail = thumbnail
            self.timestamp = timestamp
            self.destination = destination
        }

        @MainActor
        func intoVM(section: NodeSection) -> AnyTransitionVM {
            MockTransitionVM(
                transition: transition,
                section: section,
                thumbnail: thumbnail,
                timestamp: timestamp,
                destination: destination
            )
            .eraseToAnyTransitionVM()
        }

        @MainActor
        var asFavorites: AnyTransitionVM {
            intoVM(section: .favorites)
        }

        @MainActor
        var asOther: AnyTransitionVM {
            intoVM(section: .other)
        }

        @MainActor
        var asWorse: AnyTransitionVM {
            intoVM(section: .worse)
        }

        @MainActor
        var asBacklink: AnyTransitionVM {
            intoVM(section: .backlink)
        }
    }
}
