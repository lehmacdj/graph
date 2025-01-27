//
//  AnyNodeVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 7/30/23.
//

import Combine
import Foundation

@Observable final class AnyNodeVM: NodeVM {
    init(erasing underlying: NodeVM) {
        self.underlying = underlying
    }

    private var underlying: any NodeVM

    var state: Loading<NodeState> {
        get { underlying.state }
    }

    var sortOrder: NodeSortOrder {
        get { underlying.sortOrder }
        set { underlying.sortOrder = newValue }
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

    func set(tags: Set<String>) throws {
        try underlying.set(tags: tags)
    }

    func forceRemove() throws {
        try underlying.forceRemove()
    }

    func toggleFavorite(child nid: NID) throws {
        try underlying.toggleFavorite(child: nid)
    }

    func toggleWorse(child _: NID) throws {
        try underlying.toggleWorse(child: nid)
    }
}

extension NodeVM {
    func eraseToAnyNodeVM() -> AnyNodeVM {
        return AnyNodeVM(erasing: self)
    }
}
