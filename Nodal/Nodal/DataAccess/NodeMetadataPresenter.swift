//
//  MetadataObserver.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/28/25.
//

import Foundation

/// This class is `Sendable` because it only accesses its shared mutable state from the `presentedItemOperationQueue`
final class NodeMetadataPresenter: NSObject, NSFilePresenter, @unchecked Sendable {
    private var queue_url: URL

    var presentedItemURL: URL? {
        // NSFilePresenter promises to only access this from the queue we provide
        queue_url
    }

    let presentedItemOperationQueue: OperationQueue

    static func makeOperationQueue(for url: URL) -> OperationQueue {
        let queue = OperationQueue()
        queue.maxConcurrentOperationCount = 1
        queue.name = "is.devin.nodal.NodeMetadataPresenter[\(url)]"
        return queue
    }

    init(url: URL) {
        self.queue_url = url
        presentedItemOperationQueue = Self.makeOperationQueue(for: url)
        super.init()
        addOperation {
            logDebug("performing initial metadata update \(queue_url)")
            queue_updateMetadata()
        }
    }

    enum FileError: Error {
        case fileMoved
        case fileDeleted
        case failedToRead(NSError)
        case failedToGetResultWhileReading
        case errorWhileReading(Error)
    }

    private func addOperation(@_implicitSelfCapture _ block: @escaping @Sendable () -> Void) {
        presentedItemOperationQueue.addOperation(block)
    }

    func getUpdates() async -> sending any AsyncSequence<Node<NoAugmentation>, FileError> {
        await withCheckedContinuation { continuation in
            addOperation {
                let (stream, streamContinuation) = AsyncThrowingStream<Node<NoAugmentation>, any Error>.makeStream()
                let uuid = UUID()
                queue_streamContinuations[uuid] = streamContinuation
                streamContinuation.onTermination = { [uuid, weak self] _ in
                    guard let self else { return }
                    addOperation {
                        queue_streamContinuations.removeValue(forKey: uuid)
                    }
                }
                if let queue_mostRecentMetadata {
                    streamContinuation.yield(queue_mostRecentMetadata)
                } else {
                    logWarn("no most recent metadata when setting up a new update stream for \(queue_url)")
                }
                continuation.resume(returning: stream.mapFailure { $0 as! FileError })
            }
        }
    }

    private var queue_mostRecentMetadata: Node<NoAugmentation>?
    private var queue_streamContinuations = [UUID: AsyncThrowingStream<Node<NoAugmentation>, any Error>.Continuation]()

    private func queue_publish(metadata newMetadata: Node<NoAugmentation>) {
        guard newMetadata != queue_mostRecentMetadata else {
            // this is common: presentedItemDidChange is called too often
            logVerbose("no change to metadata for \(queue_url)")
            return
        }

        logDebug("publishing new metadata for \(queue_url)")
        queue_mostRecentMetadata = newMetadata
        for continuation in queue_streamContinuations.values {
            continuation.yield(newMetadata)
        }
    }

    private func queue_terminatePublishing(with error: FileError) {
        logDebug("terminating publishing for \(queue_url)")
        for continuation in queue_streamContinuations.values {
            continuation.finish(throwing: error)
        }
    }

    private func queue_updateMetadata() {
        let coordinator = NSFileCoordinator(filePresenter: self)
        let decoder = JSONDecoder()
        var result: Result<Node<NoAugmentation>, Error>?
        var error: NSError? = nil
        coordinator.coordinate(readingItemAt: queue_url, error: &error) { url in
            result = Result {
                try decoder.decode(Node<NoAugmentation>.self, from: Data(contentsOf: url))
            }
        }
        if let error {
            logError(error)
            queue_terminatePublishing(with: .failedToRead(error))
            return
        }
        guard let result else {
            logError("unexpectedly don't have result even though no error")
            queue_terminatePublishing(with: .failedToGetResultWhileReading)
            return
        }
        let newMetadata: Node<NoAugmentation>
        do {
            newMetadata = try result.get()
        } catch {
            logError(error)
            queue_terminatePublishing(with: .errorWhileReading(error))
            return
        }
        queue_publish(metadata: newMetadata)
    }

    func presentedItemDidChange() {
        logVerbose("\(queue_url) changed")
        queue_updateMetadata()
    }

    func presentedItemDidMove(to newURL: URL) {
        logWarn("metadata file moved from \(queue_url) to \(newURL)")
        queue_url = newURL
        queue_terminatePublishing(with: .fileMoved)
    }

    func accommodatePresentedItemDeletion() async throws {
        logWarn("metadata file at \(queue_url) will be deleted")
        queue_terminatePublishing(with: .fileDeleted)
    }
}
