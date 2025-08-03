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
    let dataURL: URL?
    let favoriteLinks: [AnyTransitionVM]?
    let links: [AnyTransitionVM]
    let worseLinks: [AnyTransitionVM]?
    let backlinks: [AnyTransitionVM]
    let tags: [String]
}

enum NodeSection {
    case favorites
    case worse
    case other
    case backlink

    var direction: Direction {
        switch self {
        case .favorites, .worse, .other: .forward
        case .backlink: .backward
        }
    }
}

enum NodeSortOrder: CaseIterable {
    static let allCases: [NodeSortOrder] = [
        .transitionThenTimestamp(timestampOrder: .newerFirst),
        .timestampThenTransition(timestampOrder: .olderFirst),
        .transitionThenTimestamp(timestampOrder: .newerFirst),
        .timestampThenTransition(timestampOrder: .olderFirst),
    ]

    enum TimestampOrder {
        case newerFirst
        case olderFirst
    }
    case transitionThenTimestamp(timestampOrder: TimestampOrder)
    case timestampThenTransition(timestampOrder: TimestampOrder)

    var name: String {
        switch self {
        case .transitionThenTimestamp(.newerFirst):
            "Transition, then timestamp, newer first"
        case .timestampThenTransition(.newerFirst):
            "Timestamp, newer first"
        case .timestampThenTransition(.olderFirst):
            "Timestamp, older first"
        case .transitionThenTimestamp(.olderFirst):
            "Transition, then timestamp, older first"
        }
    }

    /// we just cycle between the things that have the same timestamp order; switching timestamp order in this cycle is confusing
    /// switching between newer/older first must be accomplished by the context menu
    var next: Self {
        switch self {
        case .transitionThenTimestamp(.newerFirst):
            .timestampThenTransition(timestampOrder: .newerFirst)
        case .timestampThenTransition(.newerFirst):
            .transitionThenTimestamp(timestampOrder: .newerFirst)
        case .timestampThenTransition(.olderFirst):
            .transitionThenTimestamp(timestampOrder: .olderFirst)
        case .transitionThenTimestamp(.olderFirst):
            .timestampThenTransition(timestampOrder: .olderFirst)
        }
    }
}

@MainActor
protocol NodeVM: Observable {
    var nid: NID { get }

    var state: Loading<NodeState> { get }

    func subscribe() async

    func reload() async

    var sortOrder: NodeSortOrder { get set }

    func set(tags: Set<String>) throws

    func forceRemove() throws

    func toggleFavorite(child _: NID) throws

    func toggleWorse(child _: NID) throws
}

/// Hacky struct for adopting NavigationStack
struct NavToNode {
    let nid: NID
    let vm: AnyNodeVM
}

extension NavToNode: CustomStringConvertible {
    var description: String {
        "NavToNode:\(nid)"
    }
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
