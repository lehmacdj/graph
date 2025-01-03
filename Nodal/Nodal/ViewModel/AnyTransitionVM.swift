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

    func toggleFavorite() async {
        await underlying.toggleFavorite()
    }

    func toggleWorse() async {
        await underlying.toggleWorse()
    }

    func fetchThumbnail() {
        underlying.fetchThumbnail()
    }

    func updateTransitionName(to newName: String) async {
        await underlying.updateTransitionName(to: newName)
    }

    func removeTransition() async {
        await underlying.removeTransition()
    }
}

extension TransitionVM {
    func eraseToAnyTransitionVM() -> AnyTransitionVM {
        AnyTransitionVM(erasing: self)
    }
}
