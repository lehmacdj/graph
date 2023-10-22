//
//  SubscribingTransitionVM.swift
//  view
//
//  Created by Devin Lehmacher on 8/7/23.
//

import Foundation
import SwiftUI

@Observable class SubscribingTransitionVM<N: Node>: TransitionVM {
    let direction: Direction
    let destinationNid: NID
    var transition: String
    var thumbnail: Loading<ThumbnailValue> = .idle
    var isFavorite: Bool
    var isWorse: Bool
    // this retains the child node once loaded; thus we must weaken it if we're going to store a reference to it to avoid trying to store the entire graph in memory; it should be possible to completely avoid storing this most likely
    var destination: Loading<AnyNodeVM<N>> = .idle

    enum State {
        
    }

    private let manager: GraphManager<N>

    /// link to parent node, should always be retained because the transition is only ever presented as a part of a node
    /// NOTE: this assumption will break if I implement navigation that allows you to traverse the graph other than through child links
    private weak var parent: SubscribingNodeVM<N>!

    private let source: N

    /// The destination node if it's been acquired
    private var destinationNode: N?

    init(
        parent: SubscribingNodeVM<N>,
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

        logDebug("successfully fetched destinationNode \(destinationNid)")
        self.destination = .loaded(SubscribingNodeVM(for: destinationNid, in: manager).eraseToAnyNodeVM())
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
        do {
            try await parent.toggleFavorite(child: destinationNid)
        } catch {
            logError("failed to toggle favorite: \(error)")
        }
        isFavorite.toggle()
    }

    func toggleWorse() async {
        do {
            try await parent.toggleWorse(child: destinationNid)
        } catch {
            logError("failed to toggle worse: \(error)")
        }
        isWorse.toggle()
    }

    func fetchThumbnail() async {
        do {
            logDebug("starting to fetch thumbnail")
            let destinationNode = try destinationNode.unwrapped("destinationNode doesn't exist")
            let dataDocument = try await destinationNode.data.unwrapped("data's document initializer returned nil")
            if let image = UIImage(data: await dataDocument.data) {
                self.thumbnail = .loaded(.thumbnail(.loaded(image)))
            } else {
                self.thumbnail = .loaded(.noThumbnail)
            }
            logDebug("successfully loaded thumbnail")
        } catch {
            logError("failed to load thumbnail: \(error)")
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

extension SubscribingTransitionVM: Identifiable {
    var id: some Hashable {
        "\(transition)\(destinationNid)"
    }
}
