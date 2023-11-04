//
//  NodeVM.swift
//  view
//
//  Created by Devin Lehmacher on 3/27/23.
//

import Foundation
import Observation

protocol NodeState<N> {
    associatedtype N: Node

    var data: Data? { get set }

    var favoriteLinks: [AnyTransitionVM<N>]? { get }

    var links: [AnyTransitionVM<N>] { get }

    var worseLinks: [AnyTransitionVM<N>]? { get }

    var backlinks: [AnyTransitionVM<N>] { get }

    var tags: Set<String> { get }

    var possibleTags: Set<String> { get }
}

protocol NodeVM<N>: Observable {
    associatedtype N: Node

    var nid: NID { get }

    var state: Loading<any NodeState<N>> { get }

    @Sendable func subscribe() async

    func reload() async

    func set(tags: Set<String>) async throws

    func forceRemove() async throws

    func toggleFavorite(child _: NID) async throws

    func toggleWorse(child _: NID) async throws

    var inMemoryNodeCount: Int { get }
}
