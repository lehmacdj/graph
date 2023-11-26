//
//  LinkForTransitionVM.swift
//  view
//
//  Created by Devin Lehmacher on 3/26/23.
//

import Foundation
import SwiftUI

enum Direction {
    case forward
    case backward
}

enum ThumbnailValue {

    case noThumbnail

    /// File isn't on device so we won't attempt to generate a thumbnail
    case cloudFile

    case thumbnail(Loading<UIImage>)
}

protocol TransitionVM<N>: Observable, Identifiable {
    associatedtype N: Node

    var direction: Direction { get }

    var destinationNid: NID { get }

    var transition: String { get set }

    var thumbnail: Loading<ThumbnailValue> { get }

    var isFavorite: Bool { get set }

    var isWorse: Bool { get set }

    var destination: AnyNodeVM<N> { get }

    @Sendable func subscribe() async

    func toggleFavorite() async

    func toggleWorse() async

    func fetchThumbnail() async

    func updateTransitionName(to newName: String) async

    func removeTransition() async
}
