//
//  ProductionGraphRepository.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/4/24.
//

import Foundation
import Combine
import UIKit
import AsyncAlgorithms

actor FilesystemGraphRepository: GraphRepository {
    private let basePath: URL

    init(basePath: URL) async throws {
        self.basePath = basePath
        metadataCache = ConcurrentCache(
            create: { try await NodeMetadataDocument(metaURL: basePath.appending(metaPathFor: $0))},
            destroy: { (nid, document) in
                let result = await document.close()
                if !result {
                    logError("failed to save while closing meta document \(nid)")
                }
            }
        )
        dataDocumentCache = ConcurrentCache(
            create: { try await DataDocument(dataURL: basePath.appending(dataPathFor: $0)) },
            destroy: { (nid, document) in
                let result = await document.close()
                if !result {
                    logError("failed to save while closing data document \(nid)")
                }
            }
        )
        try await initSystemNodes()
    }

    struct FailedToInitSystemNodes: Error {
        let nodes: Set<NID>
    }

    private func initSystemNodes() async throws {
        for node in SystemNodes.allCases {
            _ = try await createNode(with: node.nid)
        }
    }

    // MARK: GraphRepository

    private func createNode(with nid: NID) async throws -> Bool {
        let newMeta = NodeMeta(id: nid, incoming: [:], outgoing: [:])
        let data: Data = try JSONEncoder().encode(newMeta)
        let metaPath = basePath.appending(metaPathFor: nid)
        do {
            try data.write(to: metaPath, options: .withoutOverwriting)
            return true
        } catch {
            let nsError = error as NSError
            if nsError.domain == NSCocoaErrorDomain && nsError.code == NSFileWriteFileExistsError {
                return false
            } else {
                throw error
            }
        }
    }

    struct NodeAlreadyExists: Error {
        let nid: NID
    }

    func createNewNode() async throws -> NID {
        let nid = NID.random()
        guard try await createNode(with: nid) else {
            throw NodeAlreadyExists(nid: nid)
        }
        return nid
    }

    func updates<T>(computeValue: @escaping ComputeValueClosure<T>) -> any AsyncSequence<T, any Error> {
        NewUpdatesSequence<T>(graphRepository: self, computeValue: computeValue)
    }

    func deleteNode(withId id: NID) async {
        logError("unimplemented")
    }

    func insertEdge(from: NID, to: NID, via transition: String) async {
        logError("unimplemented")
    }

    func deleteEdge(from: NID, to: NID, via transition: String) async {
        logError("unimplemented")
    }

    // MARK: Primitive update sequences used by the custom AsyncSequence iterators we return

    private var metadataCache: ConcurrentCache<NID, NodeMetadataDocument>

    private func getMetadataUpdates(for nid: NID) async throws -> (AnyCancellable, any AsyncSequence<NodeValue<Void>, Never>) {
        let (cacheEntry, metadataDocument) = try await metadataCache.getOrCreate(nid)
        let sequence = await metadataDocument
            .metaPublisher
            .values
            .map { NodeValue(from: $0) }
        return (cacheEntry, sequence)
    }

    private func getDataAvailabilityUpdates(for nid: NID) async throws -> (AnyCancellable, any AsyncSequence<DataAvailability, Never>) {
        let query = NSMetadataQuery()
        let url = basePath.appending(dataPathFor: nid)
        query.predicate = NSPredicate(
            format: "%K == %@",
            NSMetadataItemURLKey,
            url as CVarArg
        )
        query.searchScopes = [basePath]
        query.valueListAttributes = [NSMetadataUbiquitousItemDownloadingStatusKey]
        let (stream, continuation) = AsyncStream<DataAvailability>.makeStream()
        // we only access the query from .main so this is actually safe
        struct UnsafeSendableWrapper: @unchecked Sendable {
            let query: NSMetadataQuery
        }
        let wrappedQuery = UnsafeSendableWrapper(query: query)
        let handleResult: @Sendable (Notification) -> Void = { _ in
            wrappedQuery.query.disableUpdates()
            guard let downloadStatus = wrappedQuery.query.valueLists[url.path()]?[0] as? String else {
                continuation.yield(.noData)
                return
            }
            switch downloadStatus {
            case NSMetadataUbiquitousItemDownloadingStatusCurrent:
                continuation.yield(.availableLocally)
            case NSMetadataUbiquitousItemDownloadingStatusDownloaded:
                continuation.yield(.availableLocally)
            case NSMetadataUbiquitousItemDownloadingStatusNotDownloaded:
                continuation.yield(.availableRemotely)
            default:
                logWarn("invalid value \(downloadStatus) for NSMetadataQuery \(nid)")
                return
            }
            wrappedQuery.query.enableUpdates()
        }
        let token1 = NotificationCenter.default.addObserver(forName: .NSMetadataQueryDidFinishGathering, object: query, queue: .main, using: handleResult)
        let token2 = NotificationCenter.default.addObserver(forName: .NSMetadataQueryDidUpdate, object: query, queue: .main, using: handleResult)

        let cancellable = AnyCancellable {
            continuation.finish()
            NotificationCenter.default.removeObserver(token1)
            NotificationCenter.default.removeObserver(token2)
        }

        return (cancellable, stream)
    }

    private let dataDocumentCache: ConcurrentCache<NID, DataDocument>

    private func getDataUpdates(for nid: NID) async throws -> (AnyCancellable, any AsyncSequence<Data, Error>) {
        let (cacheEntry, dataDocument) = try await dataDocumentCache.getOrCreate(nid)
        let sequence = await dataDocument
            .dataPublisher
            .values
        return (cacheEntry, sequence)
    }
}

/// AsyncSequence implementation for `updates`
private extension FilesystemGraphRepository {
    /// Just a factory for `UpdatesIterator` which does all of the heavy lifting
    struct NewUpdatesSequence<T>: AsyncSequence {
        let graphRepository: FilesystemGraphRepository

        let computeValue: ComputeValueClosure<T>

        func makeAsyncIterator() -> NewUpdatesIterator<T> {
            NewUpdatesIterator<T>(graphRepository: graphRepository, computeValue: computeValue)
        }
    }

    actor NewUpdatesIterator<T>: AsyncIteratorProtocol {
        typealias Element = T

        let graphRepository: FilesystemGraphRepository
        let computeValue: ComputeValueClosure<T>

        init(
            graphRepository: FilesystemGraphRepository,
            computeValue: @escaping ComputeValueClosure<T>
        ) {
            self.graphRepository = graphRepository
            self.computeValue = computeValue
            self.dependencyEntries = [:]
        }

        func next() async throws -> T? {
            while true {
                while !dependencyEntriesAllComplete || !hasDependencyChangedSinceLastNextResult {
                    await waitUntilSomethingChanges()
                }

                // waitUntilSomethingChanges only works because all of this executes without any suspend points
                if let value = computeNextValue() {
                    hasDependencyChangedSinceLastNextResult = false
                    return value
                } else {
                    continue
                }
            }
        }

        private struct DM: DependencyManager {
            let untypedGet: (NID, UntypedDataNeed) throws(FetchDependencyError) -> NodeValue<UntypedDataValue>

            func fetch<D>(nid: NID, dataNeed: D) throws(FetchDependencyError) -> NodeValue<D.Value> where D : DataNeed {
                let nodeValue = try untypedGet(nid, dataNeed.untyped)
                guard let typedAugmentation = dataNeed.coerceValue(nodeValue.data) else {
                    fatalError("type mismatch: wrong augmentation type for data need \(dataNeed.untyped)")
                }
                return nodeValue.withAugmentation(typedAugmentation)
            }

            func fetch(nid: NID, untypedDataNeed: UntypedDataNeed) throws(FetchDependencyError) -> NodeValue<UntypedDataValue> {
                try untypedGet(nid, untypedDataNeed)
            }
        }

        private func computeNextValue() -> T? {
            do {
                let value = try computeValue(DM(untypedGet: trackedGetDependency(_:dataNeed:)))
                commitLargestNewRequests()
                return value
            } catch FetchDependencyError.cacheMiss {
                clearLargestNewRequests()
                return nil
            } catch FetchDependencyError.missingDependencies {
                clearLargestNewRequests()
                return nil
            } catch {
                logWarn("non-standard error thrown from computeValue: \(error)")
                clearLargestNewRequests()
                return nil
            }
        }

        // MARK: dependency management

        private struct DependencyEntry {
            init(nid: NID, dataNeed: UntypedDataNeed, graphRepository: FilesystemGraphRepository, iterator: NewUpdatesIterator<T>) {
                self.dataNeed = dataNeed
                let task = Task { [nid, graphRepository, weak iterator] in
                    do {
                        let (_, updates) = try await graphRepository.getMetadataUpdates(for: nid)
                        for await nodeValue in updates {
                            await iterator?.updateValue(nodeValue)
                        }
                        logInfo("handle.nodeValues finished")
                    } catch {
                        logError(error)
                    }
                }
                self.updateValueTask = AnyCancellable { task.cancel() }
            }

            /// Requested data need for this node
            var dataNeed: UntypedDataNeed

            /// As we run computeValue we track the data need requested on its reruns
            /// We use this to compute the maximum data need requested on the previous run which becomes the new data need
            var largestNewRequest = Max<UntypedDataNeed>()

            /// AnyCancellable that cancels a task that calls ``updateValue`` when metadataHandle emits updates for the node
            var updateValueTask: AnyCancellable

            /// Cached value from the most recent update we received from the GraphRepository.
            var mostRecentValue: NodeValue<Void>?

            private func _completeValue(withDataNeed dataNeed: UntypedDataNeed) -> NodeValue<UntypedDataValue>? {
                guard let mostRecentValue else {
                    return nil
                }

                switch dataNeed {
                case .dataNotNeeded:
                    return mostRecentValue.withAugmentation(.dataNotChecked)
                case .wantDataIfLocal:
                    // TODO: populate
                    return mostRecentValue.withAugmentation(.dataIfLocal(.noData))
                case .needDataEvenIfRemote:
                    // TODO: populate
                    return mostRecentValue.withAugmentation(.data(nil))
                }
            }

            var isValueComplete: Bool {
                _completeValue(withDataNeed: dataNeed) != nil
            }

            mutating func completeValue(withDataNeed dataNeed: UntypedDataNeed) -> NodeValue<UntypedDataValue>? {
                largestNewRequest.insert(dataNeed)
                return _completeValue(withDataNeed: dataNeed)
            }

            func resetDataNeedRequest(merge: (_ old: UntypedDataNeed, _ newMax: UntypedDataNeed) -> UntypedDataNeed) -> DependencyEntry {
                guard let newDataNeed = largestNewRequest.max else {
                    return self
                }
                var mutable = self
                mutable.dataNeed = max(mutable.dataNeed, newDataNeed)
                mutable.largestNewRequest.reset()
                return mutable
            }
        }

        /// Cleans up dependency entries after an unsuccessful run
        /// Pessimistically assumes that we may need the maximum requested by any previous failed run.
        private func clearLargestNewRequests() {
            dependencyEntries = dependencyEntries
                .mapValues { entry in entry.resetDataNeedRequest(merge: max) }
        }

        /// Cleans up dependency entries to update dependency tracking based on the latest call that we made
        /// This should be called whenever we successfully compute a value to commit the computed dependencies as a final set.
        private func commitLargestNewRequests() {
            dependencyEntries = dependencyEntries
                .filter { (_, entry) in
                    // if we didn't get a new request we no longer have a dependency on a given node
                    entry.largestNewRequest.max != nil
                }
                .mapValues { entry in entry.resetDataNeedRequest(merge: { $1 }) }
        }

        /// If an entry is in this dictionary it is a dependency.
        private var dependencyEntries: [NID: DependencyEntry]

        private var dependencyEntriesAllComplete: Bool {
            dependencyEntries.allSatisfy { $0.value.isValueComplete }
        }

        private func trackedGetDependency(_ nid: NID, dataNeed: UntypedDataNeed) throws(FetchDependencyError) -> NodeValue<UntypedDataValue> {
            guard var entry = dependencyEntries[nid] else {
                // didn't know about this dependency before, save it in the cache
                dependencyEntries[nid] = DependencyEntry(
                    nid: nid,
                    dataNeed: dataNeed,
                    graphRepository: graphRepository,
                    iterator: self
                )
                throw FetchDependencyError.cacheMiss
            }
            defer { dependencyEntries[nid] = entry }

            guard let value = entry.completeValue(withDataNeed: dataNeed) else {
                // value is not up to date yet / is missing data
                // just need to retry after waiting
                throw FetchDependencyError.cacheMiss
            }
            return value
        }

        var hasDependencyChangedSinceLastNextResult = true

        private func updateValue(_ newValue: NodeValue<Void>) {
            guard var entry = dependencyEntries[newValue.id] else {
                // maybe possible if considering concurrency shenanigans?
                logError("trying to update entry for node that doesn't exist")
                return
            }
            defer { dependencyEntries[newValue.id] = entry }

            entry.mostRecentValue = newValue
            markSomethingAsChanged()
        }

        private func markSomethingAsChanged() {
            hasDependencyChangedSinceLastNextResult = true
            if let continuation = somethingUpdatedContinuation {
                continuation.resume()
                somethingUpdatedContinuation = nil
            }
        }

        /// Wait until something changes
        /// TODO: this probably wants to handle cancellation more gracefully
        private func waitUntilSomethingChanges() async {
            assert(somethingUpdatedContinuation == nil)
            assert(!dependencyEntriesAllComplete || !hasDependencyChangedSinceLastNextResult)
            await withCheckedContinuation { continuation in
                somethingUpdatedContinuation = continuation
            }
            somethingUpdatedContinuation = nil
        }

        private var somethingUpdatedContinuation: CheckedContinuation<Void, Never>?
    }
}
