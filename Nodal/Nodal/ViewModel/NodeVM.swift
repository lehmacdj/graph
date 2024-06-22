//
//  NodeVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 3/27/23.
//

import Foundation
import Observation

protocol NodeState {
    var data: Data? { get set }

    var favoriteLinks: [AnyTransitionVM]? { get }

    var links: [AnyTransitionVM] { get }

    var worseLinks: [AnyTransitionVM]? { get }

    var backlinks: [AnyTransitionVM] { get }

    var tags: Set<String> { get }

    var possibleTags: Set<String> { get }
}

protocol NodeVM: Observable {
    var nid: NID { get }

    var state: Loading<any NodeState> { get }

    func subscribe() async

    func reload() async

    func set(tags: Set<String>) async throws

    func forceRemove() async throws

    func toggleFavorite(child _: NID) async throws

    func toggleWorse(child _: NID) async throws
}
