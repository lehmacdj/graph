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
class TransitionVM: ObservableObject {
    let direction: Direction
    @Published var transition: String
    @Published var thumbnail: Loading<Loading<UIImage>?> = .idle
    @Published var isFavorite: Bool
    @Published var isWorse: Bool
    @Published var destination: Loading<NodeVM> = .idle

    private let manager: GraphManager
    private let source: Node
    private let destinationNid: NID

    init(source: Node, transition: NodeTransition, direction: Direction, manager: GraphManager, isFavorite: Bool, isWorse: Bool) {
        self.source = source
        self.transition = transition.transition
        self.direction = direction
        self.manager = manager
        self.destinationNid = transition.nid
        self.isFavorite = isFavorite
        self.isWorse = isWorse
    }

    func load() async {
        self.thumbnail = .loading
        self.destination = .loading

        let destinationNode: Node
        do {
            destinationNode = try await manager[destinationNid].unwrapped("node \(destinationNid) doesn't exist")
        } catch {
            logError(error.localizedDescription)
            self.thumbnail = .failed(error)
            self.destination = .failed(error)
            return
        }

        logInfo("successfully fetched destinationNode \(destinationNid)")
        self.destination = .loaded(NodeVM(for: destinationNid, in: manager))

        if destinationNode.hasData {
            self.thumbnail = .loaded(nil)
        } else {
            self.thumbnail = .loaded(.loading)
            do {
                let data = try await destinationNode.data.unwrapped("checked hasData but doesn't have it")
                if let image = UIImage(data: data) {
                    self.thumbnail = .loaded(.loaded(image))
                } else {
                    self.thumbnail = .loaded(nil)
                }
            } catch {
                self.thumbnail = .loaded(.failed(error))
            }
        }
    }

    func weaken() {
        // TODO: do something so that it's possible for the thumbnail to be deallocated
        // this will help with memory usage
    }

    func toggleFavorite() async {
        // TODO: implement
    }

    func toggleWorse() async {
        // TODO: implement
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
