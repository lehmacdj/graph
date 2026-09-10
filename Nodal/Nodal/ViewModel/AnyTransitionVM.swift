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
        self.id = ObjectIdentifier(underlying)
        self.underlying = underlying
    }

    private var underlying: any TransitionVM

    /// Identity has to come from the wrapped view model, not from `self`.
    ///
    /// `GraphRepositoryNodeVM` re-erases its (reused) transition view models on
    /// every graph update, so the default `ObjectIdentifier(self)` identity
    /// would change every update and make `ForEach` tear down and rebuild every
    /// row of the list.
    nonisolated let id: ObjectIdentifier

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

    var dataURL: Loading<URL?> {
        get { underlying.dataURL }
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
