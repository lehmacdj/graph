//
//  MockTransitionVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/22/24.
//

import Foundation

@Observable
class MockTransitionVM: TransitionVM {
    let destinationNid: NID
    var transition: String
    let direction: Direction
    let thumbnail: Loading<ThumbnailValue>
    let timestamp: Loading<Date?>
    var isFavorite: Bool
    var isWorse: Bool
    let destination: AnyNodeVM

    init(
        transition: String = "",
        section: NodeSection = .other,
        thumbnail: Loading<ThumbnailValue> = .loaded(.noThumbnail),
        timestamp: Loading<Date?> = .loaded(nil),
        destination: AnyNodeVM? = nil
    ) {
        self.destinationNid = Self.nextNid()
        self.transition = transition
        self.thumbnail = thumbnail
        self.timestamp = timestamp
        self.direction = section.direction
        self.isFavorite = section == .favorites
        self.isWorse = section == .worse
        self.destination = destination ?? MockNodeVM(nid: destinationNid).eraseToAnyNodeVM()
    }

    func subscribe() async {}
    func toggleFavorite() {}
    func toggleWorse() {}
    func fetchThumbnail() {}
    func updateTransitionName(to newName: String) {}
    func removeTransition() {}

    private static var lastUsedNid = 0
    static func nextNid() -> NID {
        lastUsedNid += 1
        return NID(fake: lastUsedNid)
    }
}
