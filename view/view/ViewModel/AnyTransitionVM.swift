//
//  AnyTransitionVM.swift
//  view
//
//  Created by Devin Lehmacher on 7/30/23.
//

import Combine
import Foundation

class AnyTransitionVM<N_: Node>: TransitionVM {
    typealias N = N_

    init<VM: TransitionVM<N>>(erasing underlying: VM) {
        self.underlying = underlying
        self.underlyingChangeSubscription = underlying
            .objectWillChange
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                self?.objectWillChange.send()
            }
    }

    private var underlyingChangeSubscription: AnyCancellable?
    private var underlying: any TransitionVM<N_>

    var direction: Direction { underlying.direction }

    var destinationNid: NID { underlying.destinationNid }

    var destination: Loading<AnyNodeVM<N_>> {
        get { underlying.destination }
        set { underlying.destination = newValue }
    }

    var transition: String {
        get { underlying.transition }
        set { underlying.transition = newValue }
    }

    var thumbnail: Loading<ThumbnailValue> {
        get { underlying.thumbnail }
        set { underlying.thumbnail = newValue }
    }

    var isFavorite: Bool {
        get { underlying.isFavorite }
        set { underlying.isFavorite = newValue }
    }

    var isWorse: Bool {
        get { underlying.isWorse }
        set { underlying.isWorse = newValue }
    }

    func load() async {
        await underlying.load()
    }
    
    func weaken() {
        underlying.weaken()
    }
    
    func toggleFavorite() async {
        await underlying.toggleFavorite()
    }
    
    func toggleWorse() async {
        await underlying.toggleWorse()
    }

    func fetchThumbnail() async {
        await underlying.fetchThumbnail()
    }
    
    func updateTransitionName(to newName: String) async {
        await underlying.updateTransitionName(to: newName)
    }
    
    func removeTransition() async {
        await underlying.removeTransition()
    }
}

extension TransitionVM {
    func eraseToAnyTransitionVM() -> AnyTransitionVM<N> {
        AnyTransitionVM(erasing: self)
    }
}
