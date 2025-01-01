//
//  LinkForTransitionVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 3/26/23.
//

import Foundation
import SwiftUI

enum Direction {
    case forward
    case backward
}

enum ThumbnailValue: Equatable {
    static func == (lhs: ThumbnailValue, rhs: ThumbnailValue) -> Bool {
        switch (lhs, rhs) {
        case (.noThumbnail, .noThumbnail):
            true
        case (.cloudFile, .cloudFile):
            true
        case (.thumbnail(let lhs), .thumbnail(let rhs)):
            Loading.equalityCheck(
                loadedCheck: { $0.isEqual($1) },
                errorCheck: { "\($0)" == "\($1)" }
            )(lhs, rhs)
        default: false
        }
    }
    
    case noThumbnail

    /// File isn't on device so we won't attempt to generate a thumbnail
    case cloudFile

    case thumbnail(Loading<UIImage>)
}

@MainActor
protocol TransitionVM: Observable, Identifiable {
    var direction: Direction { get }

    var destinationNid: NID { get }

    var transition: String { get set }

    var thumbnail: Loading<ThumbnailValue> { get }

    var timestamp: Loading<Date?> { get }

    var isFavorite: Bool { get set }

    var isWorse: Bool { get set }

    var destination: AnyNodeVM { get }

    func subscribe() async

    func toggleFavorite() async

    func toggleWorse() async

    func fetchThumbnail() async

    func updateTransitionName(to newName: String) async

    func removeTransition() async
}
