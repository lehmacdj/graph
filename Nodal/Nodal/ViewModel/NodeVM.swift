//
//  NodeVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 3/27/23.
//

import Foundation
import Observation

struct NodeState {
    let data: Data?
    let favoriteLinks: [AnyTransitionVM]?
    let links: [AnyTransitionVM]
    let worseLinks: [AnyTransitionVM]?
    let backlinks: [AnyTransitionVM]
    let tags: Set<String>
    let possibleTags: Set<String>
}

@MainActor
protocol NodeVM: Observable {
    var nid: NID { get }

    var state: Loading<NodeState> { get }

    func subscribe() async

    func reload() async

    func set(tags: Set<String>) async throws

    func forceRemove() async throws

    func toggleFavorite(child _: NID) async throws

    func toggleWorse(child _: NID) async throws
}

/// Hacky struct for adopting NavigationStack
struct NavToNode {
    let nid: NID
    let vm: AnyNodeVM
}


extension NavToNode: Equatable {
    static func == (lhs: NavToNode, rhs: NavToNode) -> Bool {
        lhs.nid == rhs.nid
    }
}

extension NavToNode: Hashable {
    func hash(into hasher: inout Hasher) {
        nid.hash(into: &hasher)
    }
}

extension NodeVM {
    var nav: NavToNode {
        NavToNode(nid: nid, vm: eraseToAnyNodeVM())
    }
}
