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
        sourceNid: NID,
        transition: NodeTransition,
        timestamp: Loading<Date?>,
        configuredForSection section: NodeSection
    ) {
        self.graphRepository = graphRepository
        self.sourceNid = sourceNid
        self.destinationNid = transition.nid
        self.transition = transition.transition
        self.timestamp = timestamp
        self.direction = section.direction
        self.isFavorite = section == .favorites
        self.isWorse = section == .worse
    }

    let sourceNid: NID
    let destinationNid: NID
    var direction: Direction
    var transition: String
    var isFavorite: Bool
    var isWorse: Bool

    var destination: AnyNodeVM {
        GraphRepositoryNodeVM(nid: destinationNid, graphRepository: graphRepository)
            .eraseToAnyNodeVM()
    }

    var description: String {
        let d = direction == .forward ? ">" : "<"
        return "TransitionVM(\(sourceNid)\(d)\(transition)\(d)\(destinationNid))"
    }

    func subscribe() async {
        await withTaskGroup(of: Void.self) { group in
            withPushLogContext(description + ".subscribeThumbnail") {
                group.addTask { await self.subscribeThumbnail() }
            }
            withPushLogContext(description + ".subscribeTags") {
                group.addTask { await self.subscribeTags() }
            }
        }
    }

    var thumbnail: Loading<ThumbnailValue> = .loaded(.noThumbnail)

    /// Whether we require the thumbnail to be available locally
    /// Once set to true this should remain true
    private var requireThumbnail = false

    /// Task that tries to fetch the thumbnail
    /// ``runThumbnailSubscription()`` continuously restarts this task so that we can cancel
    /// from ``fetchThumbnail()`` to respect the ``requireThumbnail`` property
    private var updateThumbnailTask: Task<Void, Never>?

    /// The in-flight ``subscribeThumbnail()`` call, if there is one.
    ///
    /// SwiftUI cancels a row's `.task` and starts its replacement immediately, so a new
    /// subscription regularly begins while the previous one is still tearing down. Waiting
    /// for the previous one to finish (instead of returning) is what keeps the cell
    /// subscribed: a dropped subscription leaves it with a stale thumbnail and no update
    /// loop for ``fetchThumbnail()`` to restart, so tapping it does nothing at all until
    /// scrolling recreates the row.
    private var thumbnailSubscription: Task<Void, Never>?

    private func subscribeThumbnail() async {
        while let previous = thumbnailSubscription {
            // `previous` clears the slot from its own `defer`, so it is nil by the time we
            // are resumed unless another subscription got there first
            await previous.value
            if Task.isCancelled { return }
        }
        let subscription = Task { await runThumbnailSubscription() }
        thumbnailSubscription = subscription
        await withTaskCancellationHandler {
            await subscription.value
        } onCancel: {
            subscription.cancel()
        }
    }

    private func runThumbnailSubscription() async {
        logDebug("beginning subscription")
        defer {
            logDebug("ending subscription")
            thumbnailSubscription = nil
        }
        while !Task.isCancelled {
            // this is trying to:
            // 1. continuously (re-)start the task if it gets cancelled but the parent task (called from SwiftUI isn't)
            // 2. respond to cancellation by stopping any processes that are continuously trying to update the VM
            let task = Task { await updateThumbnailLoop() }
            updateThumbnailTask = task
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

    private func updateThumbnailLoop() async {
        defer {
            updateThumbnailTask = nil
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
                case .dataNotChecked, .availabilityOnly(_):
                    fatalError("we always request at least .wantDataIfLocal")
                case .data(let .some(dataWithURL)), .dataIfLocal(.localData(let dataWithURL)):
                    // computing this UIImage might be too heavy to do on the main thread
                    if let image = UIImage(data: dataWithURL.data) {
                        thumbnail = .loaded(.thumbnail(.loaded(image)))
                    } else {
                        thumbnail = .loaded(.noThumbnail)
                    }
                    dataURL = .loaded(dataWithURL.url)
                case .data(nil), .dataIfLocal(.noData):
                    thumbnail = .loaded(.noThumbnail)
                    dataURL = .loaded(nil)
                case .dataIfLocal(.remoteDataExists(let url)):
                    thumbnail = .loaded(.cloudFile)
                    dataURL = .loaded(url)
                }
            }
            logDebug("exited update loop (Task.isCancelled=\(Task.isCancelled))")
        } catch {
            logError(error)
            thumbnail = .failed(error)
        }
    }

    var timestamp: Loading<Date?>

    var tags: Loading<[String]> = .idle

    var dataURL: Loading<URL?> = .idle

    private var isSubscribedToTags = false

    private func subscribeTags() async {
        defer {
            isSubscribedToTags = false
            switch tags {
            case .loading: tags = .idle
            default: break
            }
        }
        switch tags {
        case .idle: tags = .loading
        case .failed: return
        case .loaded, .loading: break
        }

        do {
            let tagUpdates = await graphRepository.updates(computeValue: destinationNid.computeNodeWithTags)
            for try await nodeWithTags in tagUpdates {
                tags = .loaded(nodeWithTags.tags.sorted())
            }
        } catch {
            logError(error)
            tags = .failed(error)
        }
    }

    func fetchThumbnail() {
        withPushLogContext(description + ".fetchThumbnail", operation: _fetchThumbnail)
    }

    func _fetchThumbnail() {
        guard case .loaded(.cloudFile) = thumbnail else {
            logWarn("can't fetch thumbnail unless it is available remotely")
            return
        }
        // Show the spinner before doing anything else. The update loop may not be running
        // yet, and a download requested by an earlier tap can still be in flight, so
        // leaving the cell showing the cloud icon makes the tap look ignored until the
        // download happens to complete.
        thumbnail = .loaded(.thumbnail(.loading))
        guard !requireThumbnail else {
            logInfo("thumbnail fetch was already requested; waiting for it")
            return
        }
        requireThumbnail = true
        if let updateThumbnailTask {
            // cancelling makes the subscription restart the loop, which then requests the
            // data with .needDataEvenIfRemote
            updateThumbnailTask.cancel()
        } else {
            // requireThumbnail is sticky, so the next update loop still downloads the data
            logWarn("no update thumbnail task to cancel")
        }
    }

    // MARK: unimplemented for now because I don't support mutation right now

    func toggleFavorite() {}

    func toggleWorse() {}

    func updateTransitionName(to newName: String) {}

    func removeTransition() {}

    // MARK: special interop for GraphRepositoryNodeVM

    func configureForSection(_ section: NodeSection) {
        isFavorite = section == .favorites
        isWorse = section == .worse
        direction = section.direction
    }
}

extension GraphRepositoryTransitionVM {
    func logContext() -> [String] {
        let d = self.direction == .forward ? ">" : "<"
        let description = "\(type(of: self)):\(sourceNid)\(d)\(transition)\(d)\(destinationNid))"
        return [description]
    }
}
