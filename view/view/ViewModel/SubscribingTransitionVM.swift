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
    var destination: AnyNodeVM<N> {
        SubscribingNodeVM(for: destinationNid, in: manager).eraseToAnyNodeVM()
    }

    private var destinationNode: N? = nil

    private let manager: GraphManager<N>

    /// link to parent node, should always be retained because the transition is only ever presented as a part of a node
    /// NOTE: this assumption will break if I implement navigation that allows you to traverse the graph other than through child links
    private weak var parent: SubscribingNodeVM<N>!

    private let source: N

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

    private func fetchDestinationNode() async {
        let destinationNode: N
        do {
            destinationNode = try await manager[destinationNid]
            self.destinationNode = destinationNode
            logDebug("successfully fetched destinationNode \(destinationNid)")
        } catch {
            logError(error.localizedDescription)
            self.thumbnail = .failed(error)
            return
        }
    }

    private func loadThumbnail() async {
        guard let destinationNode else { return }

        if destinationNode.dataURL == nil {
            self.thumbnail = .loaded(.noThumbnail)
        } else if destinationNode.dataRequiresDownload {
            self.thumbnail = .loaded(.cloudFile)
        } else {
            await fetchThumbnail()
        }
    }

    func subscribe() async {
        await fetchDestinationNode()

        switch thumbnail {
        case .idle:
            self.thumbnail = .loading
            await loadThumbnail()
        case .loading:
            logError("subscribe should only be called once")
            return
        case .loaded:
            logInfo("skipping loading because already loaded")
            return
        case .failed:
            logInfo("skipping loaded because we failed to load before")
        }

        self.destinationNode = nil

        do {
            while true {
                // TODO: we probably want to eventually update the thumbnail automatically here
                // at the moment we don't have a great way of doing so, so we just spin so we can deallocate destinationNode when the transition goes out of scope
                try await Task.sleep(for: .seconds(1))
            }
        } catch is CancellationError {
            logDebug("cancelled transition: \(destinationNid)")
        } catch {
            logError("unexpected error: \(error)")
        }
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
            let destinationNode = try destinationNode.unwrapped("destinationNode was nil")
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

extension SubscribingTransitionVM: CustomDebugStringConvertible {
    var debugDescription: String {
        "\(parent?.nid.description ?? "some parent") -> \(destinationNid) via \(transition)"
    }
}
