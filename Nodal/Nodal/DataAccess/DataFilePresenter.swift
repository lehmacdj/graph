//
//  DataFileObserver.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/28/25.
//

import Foundation

/// This class is `Sendable` because it only accesses its shared mutable state from the `presentedItemOperationQueue`
final class DataFilePresenter: NSObject, NSFilePresenter, @unchecked Sendable {
    private var queue_url: URL

    var presentedItemURL: URL? {
        // NSFilePresenter promises to only access this from the queue we provide
        queue_url
    }

    let presentedItemOperationQueue: OperationQueue

    static func makeOperationQueue(for url: URL) -> OperationQueue {
        let queue = OperationQueue()
        queue.maxConcurrentOperationCount = 1
        queue.name = "is.devin.nodal.DataFilePresenter[\(url)]"
        return queue
    }

    init(url: URL) {
        self.queue_url = url
        presentedItemOperationQueue = Self.makeOperationQueue(for: url)
        super.init()
        addOperation {
            logDebug("performing initial data availability update \(queue_url)")
            queue_updateDataAvailability()
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

    private func queue_terminatePublishing(with error: FileError) {
        logDebug("terminating publishing for \(queue_url)")
        for continuation in queue_dataAvailabilityStreamContinuations.values {
            continuation.finish(throwing: error)
        }
        for continuation in queue_dataStreamContinuations.values {
            continuation.finish(throwing: error)
        }
    }

    private var queue_mostRecentDataAvailability: DataAvailability?
    private var queue_dataAvailabilityStreamContinuations = [UUID: AsyncThrowingStream<DataAvailability, any Error>.Continuation]()

    private func queue_publish(dataAvailability newDataAvailability: DataAvailability) {
        guard newDataAvailability != queue_mostRecentDataAvailability else {
            logVerbose("publishing without change \(queue_url)")
            return
        }

        queue_mostRecentDataAvailability = newDataAvailability
        logDebug("publishing new data availability for \(queue_url)")
        for continuation in queue_dataAvailabilityStreamContinuations.values {
            continuation.yield(newDataAvailability)
        }
    }

    func getDataAvailabilityUpdates() async -> sending any AsyncSequence<DataAvailability, FileError> {
        await withCheckedContinuation { continuation in
            addOperation {
                let (stream, streamContinuation) = AsyncThrowingStream<DataAvailability, any Error>.makeStream()
                let uuid = UUID()
                queue_dataAvailabilityStreamContinuations[uuid] = streamContinuation
                streamContinuation.onTermination = { [uuid, weak self] _ in
                    guard let self else { return }
                    addOperation {
                        queue_dataAvailabilityStreamContinuations.removeValue(forKey: uuid)
                    }
                }
                if let queue_mostRecentDataAvailability {
                    streamContinuation.yield(queue_mostRecentDataAvailability)
                } else {
                    logWarn("no most recent metadata when setting up a new update stream for \(queue_url)")
                }
                continuation.resume(returning: stream.mapFailure { $0 as! FileError })
            }
        }
    }

    func queue_updateDataAvailability() {
        let coordinator = NSFileCoordinator(filePresenter: self)
        var result: Result<URLResourceValues, Error>?
        var error: NSError? = nil
        coordinator.coordinate(readingItemAt: queue_url, options: [.immediatelyAvailableMetadataOnly], error: &error) { url in
            result = Result {
                try url.resourceValues(forKeys: [.ubiquitousItemDownloadingStatusKey])
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
        let newDataAvailability: DataAvailability
        do {
            switch try result.get().ubiquitousItemDownloadingStatus {
            case nil:
                newDataAvailability = .noData
            case .notDownloaded:
                newDataAvailability = .availableRemotely(queue_url)
            case .downloaded:
                newDataAvailability = .availableLocally(queue_url)
            case .current:
                newDataAvailability = .availableLocally(queue_url)
            case .some(let status):
                logError("unexpected download status \(status)")
                struct UnexpectedDownloadStatusKey: Error {}
                throw UnexpectedDownloadStatusKey()
            }
        } catch {
            logError(error)
            queue_terminatePublishing(with: .errorWhileReading(error))
            return
        }
        queue_publish(dataAvailability: newDataAvailability)
    }

    private var queue_mostRecentData: Data?
    private var queue_dataStreamContinuations = [UUID: AsyncThrowingStream<Data, any Error>.Continuation]()

    private func queue_publish(data newData: Data) {
        guard newData != queue_mostRecentData else {
            // this is common: presentedItemDidChange is called too often
            logVerbose("publishing without change \(queue_url)")
            return
        }

        queue_mostRecentData = newData
        logDebug("publishing new data for \(queue_url)")
        for continuation in queue_dataStreamContinuations.values {
            continuation.yield(newData)
        }
    }

    func getDataUpdates() async -> sending any AsyncSequence<Data, FileError> {
        await withCheckedContinuation { continuation in
            addOperation {
                let (stream, streamContinuation) = AsyncThrowingStream<Data, any Error>.makeStream()
                let uuid = UUID()
                queue_dataStreamContinuations[uuid] = streamContinuation
                streamContinuation.onTermination = { [uuid, weak self] _ in
                    guard let self else { return }
                    addOperation {
                        queue_dataAvailabilityStreamContinuations.removeValue(forKey: uuid)
                    }
                }
                if let queue_mostRecentData {
                    streamContinuation.yield(queue_mostRecentData)
                } else {
                    logDebug("data for \(queue_url) is nil, scheduling data update")
                    addOperation { queue_updateData() }
                }
                continuation.resume(returning: stream.mapFailure { $0 as! FileError })
            }
        }
    }

    private func queue_updateData() {
        let coordinator = NSFileCoordinator(filePresenter: self)
        var result: Result<Data, Error>?
        var error: NSError? = nil
        coordinator.coordinate(readingItemAt: queue_url, error: &error) { url in
            result = Result {
                try Data(contentsOf: url)
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
        let newData: Data
        do {
            newData = try result.get()
        } catch {
            logError(error)
            queue_terminatePublishing(with: .errorWhileReading(error))
            return
        }
        queue_publish(data: newData)
    }

    func presentedItemDidChange() {
        logDebug("\(queue_url) changed")
        queue_updateData()
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

    let observedPresentedItemUbiquityAttributes: Set<URLResourceKey> = [.ubiquitousItemDownloadingStatusKey]

    func presentedItemDidChangeUbiquityAttributes(_: Set<URLResourceKey>) {
        queue_updateDataAvailability()
    }

    func accommodatePresentedItemEviction() async throws {
        logDebug("\(queue_url) being evicted")
        queue_mostRecentData = nil
    }
}
