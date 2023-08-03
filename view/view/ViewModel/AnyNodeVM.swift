//
//  AnyNodeVM.swift
//  view
//
//  Created by Devin Lehmacher on 7/30/23.
//

import Combine
import Foundation

class AnyNodeVM<N_: Node>: NodeVM {
    typealias N = N_

    init<VM: NodeVM<N>>(erasing underlying: VM) {
        self.underlying = underlying
        self.underlyingChangeSubscription = underlying.objectWillChange.sink { [weak self] _ in
            self?.objectWillChange.send()
        }
    }

    private var underlyingChangeSubscription: AnyCancellable?
    private var underlying: any NodeVM<N_>

    var state: Loading<any NodeState<N_>> {
        get { underlying.state }
        set { underlying.state = newValue }
    }

    var nid: NID {
        get { underlying.nid }
    }

    func load() async {
        await underlying.load()
    }
    
    func toggleFavorite(child: NID) async {
        await underlying.toggleFavorite(child: child)
    }
    
    func toggleWorse(child: NID) async {
        await underlying.toggleWorse(child: child)
    }
}
