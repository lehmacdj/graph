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
        to destinationNid: NID,
        transition: String = "Transition name",
        direction: Direction = .forward,
        thumbnail: Loading<ThumbnailValue> = .loaded(.noThumbnail),
        timestamp: Loading<Date?> = .loaded(nil),
        isFavorite: Bool = false,
        isWorse: Bool = false,
        destination: AnyNodeVM? = nil
    ) {
        self.direction = direction
        self.destinationNid = destinationNid
        self.transition = transition
        self.thumbnail = thumbnail
        self.timestamp = timestamp
        self.isFavorite = isFavorite
        self.isWorse = isWorse
        self.destination = destination ?? MockNodeVM(nid: destinationNid).eraseToAnyNodeVM()
    }

    func subscribe() async {}
    func toggleFavorite() {}
    func toggleWorse() {}
    func fetchThumbnail() {}
    func updateTransitionName(to newName: String) {}
    func removeTransition() {}
}
