//
//  AnyNodeVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 7/30/23.
//

import Combine
import Foundation

@Observable final class AnyNodeVM<N_: Node>: NodeVM {
    typealias N = N_

    init<VM: NodeVM<N>>(erasing underlying: VM) {
        self.underlying = underlying
    }

    private var underlying: any NodeVM<N_>

    var state: Loading<any NodeState<N_>> {
        get { underlying.state }
    }

    var nid: NID {
        get { underlying.nid }
    }

    func subscribe() async {
        await underlying.subscribe()
    }

    func reload() async {
        await underlying.reload()
    }

    func set(tags: Set<String>) async throws {
        try await underlying.set(tags: tags)
    }

    func forceRemove() async throws {
        try await underlying.forceRemove()
    }

    func toggleFavorite(child nid: NID) async throws {
        try await underlying.toggleFavorite(child: nid)
    }

    func toggleWorse(child _: NID) async throws {
        try await underlying.toggleWorse(child: nid)
    }
}

extension NodeVM {
    func eraseToAnyNodeVM() -> AnyNodeVM<N> {
        return AnyNodeVM(erasing: self)
    }
}
