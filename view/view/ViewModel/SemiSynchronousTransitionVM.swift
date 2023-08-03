//
//  SemiSynchronousTransitionVM.swift
//  view
//
//  Created by Devin Lehmacher on 7/21/23.
//

import Foundation
import SwiftUI

class SemiSynchronousTransitionVM<N: Node>: ObservableObject, TransitionVM {
    let direction: Direction
    let destinationNid: NID
    @Published var transition: String
    @Published var thumbnail: Loading<ThumbnailValue> = .idle
    @Published var isFavorite: Bool
    @Published var isWorse: Bool
    @Published var destination: Loading<AnyNodeVM<N>> = .idle

    private let manager: GraphManager<N>

    /// link to parent node, should always be retained because the transition is only ever presented as a part of a node
    private weak var parent: SemiSynchronousNodeVM<N>!

    private let source: N

    /// The destination node if it's been acquired
    private var destinationNode: N?

    init(
        parent: SemiSynchronousNodeVM<N>,
        source: N,
        transition: NodeTransition,
        direction: Direction,
        manager: GraphManager<N>,
        isFavorite: Bool,
        isWorse: Bool
    ) {
        self.parent = parent
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
            destinationNode = try await manager[destinationNid]
        } catch {
            logError(error.localizedDescription)
            self.thumbnail = .failed(error)
            self.destination = .failed(error)
            self.destinationNode = nil
            return
        }

        logInfo("successfully fetched destinationNode \(destinationNid)")
        self.destination = await .loaded(SemiSynchronousNodeVM(for: destinationNid, in: manager).eraseToAnyNodeVM())
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
//        destinationNode = nil
    }

    func toggleFavorite() async {
        await parent.toggleFavorite(child: destinationNid)
        isFavorite.toggle()
    }

    func toggleWorse() async {
        await parent.toggleWorse(child: destinationNid)
        isWorse.toggle()
    }

    func fetchThumbnail() async {
        do {
            let destinationNode = try destinationNode.unwrapped("destinationNode doesn't exist")
            let dataDocument = try await destinationNode.data.unwrapped("data's document initializer returned nil")
            if let image = UIImage(data: await dataDocument.data) {
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

extension SemiSynchronousTransitionVM: Identifiable {
    var id: some Hashable {
        "\(transition)\(destinationNid)"
    }
}
