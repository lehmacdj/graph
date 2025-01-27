//
//  AnyTransitionVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 7/30/23.
//

import Combine
import Foundation

@Observable final class AnyTransitionVM: TransitionVM {
    init(erasing underlying: any TransitionVM) {
        self.underlying = underlying
    }

    private var underlying: any TransitionVM

    var direction: Direction { underlying.direction }

    var destinationNid: NID { underlying.destinationNid }

    var destination: AnyNodeVM {
        underlying.destination
    }

    var transition: String {
        get { underlying.transition }
        set { underlying.transition = newValue }
    }

    var thumbnail: Loading<ThumbnailValue> {
        get { underlying.thumbnail }
    }

    var timestamp: Loading<Date?> {
        get { underlying.timestamp }
    }

    var tags: Loading<[String]> {
        get { underlying.tags }
    }

    var isFavorite: Bool {
        get { underlying.isFavorite }
        set { underlying.isFavorite = newValue }
    }

    var isWorse: Bool {
        get { underlying.isWorse }
        set { underlying.isWorse = newValue }
    }

    func subscribe() async {
        await underlying.subscribe()
    }

    func toggleFavorite() {
        underlying.toggleFavorite()
    }

    func toggleWorse() {
        underlying.toggleWorse()
    }

    func fetchThumbnail() {
        underlying.fetchThumbnail()
    }

    func updateTransitionName(to newName: String) {
        underlying.updateTransitionName(to: newName)
    }

    func removeTransition() {
        underlying.removeTransition()
    }
}

extension TransitionVM {
    func eraseToAnyTransitionVM() -> AnyTransitionVM {
        AnyTransitionVM(erasing: self)
    }
}
