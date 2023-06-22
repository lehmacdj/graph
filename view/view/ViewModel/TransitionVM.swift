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

@MainActor
class TransitionVM<N: Node>: ObservableObject {
    let direction: Direction
    @Published var transition: String
    @Published var thumbnail: Loading<ThumbnailValue> = .idle
    @Published var isFavorite: Bool
    @Published var isWorse: Bool
    @Published var destination: Loading<NodeVM<N>> = .idle

    enum ThumbnailValue {
        case noThumbnail

        /// File isn't on device so we won't attempt to generate a thumbnail
        case cloudFile

        case thumbnail(Loading<UIImage>)
    }

    private let manager: GraphManager<N>
    private let source: N
    private let destinationNid: NID

    /// The destination node if it's been acquired
    private var destinationNode: N?

    init(source: N, transition: NodeTransition, direction: Direction, manager: GraphManager<N>, isFavorite: Bool, isWorse: Bool) {
        self.source = source
        self.transition = transition.transition
        self.direction = direction
        self.manager = manager
        self.destinationNid = transition.nid
        self.isFavorite = isFavorite
        self.isWorse = isWorse
    }

    func load() async {
        guard case .idle = destination else {
            return
        }

        self.thumbnail = .loading
        self.destination = .loading

        let destinationNode: N
        do {
            destinationNode = try await manager[destinationNid].unwrapped("node \(destinationNid) doesn't exist")
        } catch {
            logError(error.localizedDescription)
            self.thumbnail = .failed(error)
            self.destination = .failed(error)
            self.destinationNode = nil
            return
        }

        logInfo("successfully fetched destinationNode \(destinationNid)")
        self.destination = .loaded(NodeVM(for: destinationNid, in: manager))
        self.destinationNode = destinationNode

        if destinationNode.dataURL == nil {
            self.thumbnail = .loaded(.noThumbnail)
        } else if destinationNode.dataRequiresDownload {
            self.thumbnail = .loaded(.cloudFile)
        } else {
            await fetchThumbnail()
        }
    }

    func weaken() {
        destinationNode = nil
    }

    func toggleFavorite() async {
        // TODO: implement
    }

    func toggleWorse() async {
        // TODO: implement
    }

    func fetchThumbnail() async {
        do {
            let data = try (await destinationNode?.data?.data).unwrapped("data doesn't exist")
            if let image = UIImage(data: data) {
                self.thumbnail = .loaded(.thumbnail(.loaded(image)))
            } else {
                self.thumbnail = .loaded(.noThumbnail)
            }
        } catch {
            self.thumbnail = .loaded(.thumbnail(.failed(error)))
        }
    }

    func updateTransitionName(to newName: String) async {
        await manager.removeLink(from: source.nid, to: destinationNid, via: transition)
        await manager.addLink(from: source.nid, to: destinationNid, via: newName)
    }

    func removeTransition() async {
        await manager.removeLink(from: source.nid, to: destinationNid, via: transition)
    }
}

extension TransitionVM: Identifiable {
    var id: some Hashable {
        "\(transition)\(destinationNid)"
    }
}
