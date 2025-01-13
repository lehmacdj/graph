//
//  GraphRepositoryTransitionVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 12/15/24.
//

import Foundation
import UIKit

@Observable
final class GraphRepositoryTransitionVM: TransitionVM {
    private let graphRepository: GraphRepository

    init(
        graphRepository: GraphRepository,
        transition: NodeTransition,
        timestamp: Loading<Date?>,
        configuredForSection section: GraphRepositoryNodeVM.NodeSection
    ) {
        self.graphRepository = graphRepository
        self.destinationNid = transition.nid
        self.transition = transition.transition
        self.timestamp = timestamp
        self.direction = section.direction
        self.isFavorite = section == .favorites
        self.isWorse = section == .worse
    }

    let destinationNid: NID
    var direction: Direction
    var transition: String
    var isFavorite: Bool
    var isWorse: Bool

    var destination: AnyNodeVM {
        GraphRepositoryNodeVM(nid: destinationNid, graphRepository: graphRepository)
            .eraseToAnyNodeVM()
    }

    var subscribeThumbnailTask: Task<Void, Never>?

    func subscribe() async {
        guard subscribeThumbnailTask == nil else { return }
        defer { subscribeThumbnailTask = nil }
        while !Task.isCancelled {
            // this is trying to:
            // 1. continuously (re-)start the task if it gets cancelled but the parent task (called from SwiftUI isn't)
            // 2. respond to cancellation by stopping any processes that are continuously trying to update the VM
            let task: Task<Void, Never>
            if let subscribeThumbnailTask {
                task = subscribeThumbnailTask
            } else {
                task = Task { await updateThumbnailLoop() }
            }
            subscribeThumbnailTask = task
            await withTaskCancellationHandler {
                await task.value
            } onCancel: {
                task.cancel()
            }
            if !task.isCancelled {
                // if task wasn't cancelled return indicates thumbnail loop finished finished
                return
            }
        }
    }

    var thumbnail: Loading<ThumbnailValue> = .loaded(.noThumbnail)

    private func updateThumbnailLoop() async {
        defer {
            subscribeThumbnailTask = nil
            switch thumbnail {
            case .loading: thumbnail = .idle
            default: break
            }
        }
        switch thumbnail {
        case .idle: thumbnail = .loading
        case .failed: return
        case .loaded, .loading: break
        }

        do {
            let updatesSequence = await graphRepository.updates { [destinationNid, requireThumbnail] dependencyManager in
                return try dependencyManager.fetch(
                    nid: destinationNid,
                    untypedDataNeed: requireThumbnail ? .needDataEvenIfRemote : .wantDataIfLocal
                ).data
            }
            for try await update in updatesSequence {
                switch update {
                case .dataNotChecked:
                    fatalError("we always request at least .wantDataIfLocal")
                case .data(let .some(data)), .dataIfLocal(.localData(let data)):
                    // computing this UIImage might be too heavy to do on the main thread
                    if let image = UIImage(data: data) {
                        thumbnail = .loaded(.thumbnail(.loaded(image)))
                    } else {
                        thumbnail = .loaded(.noThumbnail)
                    }
                case .data(nil), .dataIfLocal(.noData):
                    thumbnail = .loaded(.noThumbnail)
                case .dataIfLocal(.remoteDataExists):
                    thumbnail = .loaded(.cloudFile)
                }
            }
        } catch {
            logError(error)
            thumbnail = .failed(error)
        }
    }

    var timestamp: Loading<Date?>

    /// Whether we require the thumbnail to be available locally
    /// Once set to true this should remain true
    var requireThumbnail = false

    func fetchThumbnail() {
        guard !requireThumbnail else {
            logWarn("trying to fetch thumbnail a second time")
            return
        }
        requireThumbnail = true
        subscribeThumbnailTask?.cancel()
    }

    // MARK: unimplemented for now because I don't support mutation right now

    func toggleFavorite() async {}

    func toggleWorse() async {}

    func updateTransitionName(to newName: String) async {}

    func removeTransition() async {}

    // MARK: special interop for GraphRepositoryNodeVM

    func configureForSection(_ section: GraphRepositoryNodeVM.NodeSection) {
        isFavorite = section == .favorites
        isWorse = section == .worse
        direction = section.direction
    }
}
