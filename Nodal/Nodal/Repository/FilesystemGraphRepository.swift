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
            create: { try await NodeMetadataDocument(metaURL: basePath.appending(metaPathFor: $0)) },
            destroy: { (nid, document) in
                let result = await document.close()
                if !result {
                    logError("failed to save while closing meta document \(nid)")
                }
            },
            logContext: "metadataCache"
        )
        dataAvailabilityCache = ConcurrentCache(
            create: { FileAvailabilityObserver(url: basePath.appending(dataPathFor: $0)) },
            destroy: { (nid, observer) in },
            logContext: "dataAvailabilityCache"
        )
        dataDocumentCache = ConcurrentCache(
            create: { try await DataDocument(dataURL: basePath.appending(dataPathFor: $0)) },
            destroy: { (nid, document) in
                let result = await document.close()
                if !result {
                    logError("failed to save while closing data document \(nid)")
                }
            },
            logContext: "dataDocumentCache"
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
        UpdatesSequence<T>(graphRepository: self, computeValue: computeValue)
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

    private func getMetadataUpdates(for nid: NID) async throws -> (AnyCancellable, any AsyncSequence<NodeValue<NoAugmentation>, Never>) {
        let (cacheEntry, metadataDocument) = try await metadataCache.getOrCreate(nid)
        let sequence = await metadataDocument
            .metaPublisher
            .values
            .map { NodeValue(from: $0) }
        return (cacheEntry, sequence)
    }

    private var dataAvailabilityCache: ConcurrentCache<NID, FileAvailabilityObserver>

    private func getDataAvailabilityUpdates(for nid: NID) async throws -> (AnyCancellable, any AsyncSequence<DataAvailability, Never>) {
        let (cacheEntry, observer) = try await dataAvailabilityCache.getOrCreate(nid)
        let updates = await observer.updates
        return (cacheEntry, updates)
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
    struct UpdatesSequence<T>: AsyncSequence {
        let graphRepository: FilesystemGraphRepository

        let computeValue: ComputeValueClosure<T>

        func makeAsyncIterator() -> UpdatesIterator<T> {
            UpdatesIterator<T>(graphRepository: graphRepository, computeValue: computeValue)
        }
    }

    actor UpdatesIterator<T>: AsyncIteratorProtocol, LogContextProviding {
        typealias Element = T

        let graphRepository: FilesystemGraphRepository
        let computeValue: ComputeValueClosure<T>
        let logContext: [String]

        init(
            graphRepository: FilesystemGraphRepository,
            computeValue: @escaping ComputeValueClosure<T>
        ) {
            self.graphRepository = graphRepository
            self.computeValue = computeValue
            self.dependencyEntries = [:]
            let logId = Base62Id.random(digitCount: 4)
            self.logContext = ["UpdatesIterator:\(logId)"]
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
            let untypedGet: (NID, DataNeed) throws(FetchDependencyError) -> NodeValue<UntypedDataValue>

            func fetch<D>(nid: NID, dataNeed: D) throws(FetchDependencyError) -> NodeValue<D.Value> where D : TypedDataNeed {
                let nodeValue = try untypedGet(nid, dataNeed.untyped)
                guard let typedAugmentation = dataNeed.coerceValue(nodeValue.data) else {
                    fatalError("type mismatch: wrong augmentation type for data need \(dataNeed.untyped)")
                }
                return nodeValue.withAugmentation(typedAugmentation)
            }

            func fetch(nid: NID, untypedDataNeed: DataNeed) throws(FetchDependencyError) -> NodeValue<UntypedDataValue> {
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

        // theoretically I would like this to be ~Copyable because it manages some subscriptions that could cause memory leaks if doubled
        // Currently however the Swift standard library barely supports ~Copyable so using it is too painful to be worth doing
        private struct DependencyEntry {
            let nid: NID
            let graphRepository: FilesystemGraphRepository
            weak var iterator: UpdatesIterator<T>?

            init(nid: NID, dataNeed: DataNeed, graphRepository: FilesystemGraphRepository, iterator: UpdatesIterator<T>) {
                self.nid = nid
                self.dataNeed = dataNeed
                self.graphRepository = graphRepository
                self.iterator = iterator
                // this needs to be set for the first run where we don't call completeValue too
                self.largestNewDataNeed.insert(dataNeed)
                let task = Task { [nid, graphRepository, weak iterator] in
                    do {
                        iterator?.logDebug("setting up metadata updates for \(nid)")
                        let (cancellable, updates) = try await graphRepository.getMetadataUpdates(for: nid)
                        for await nodeValue in updates {
                            await iterator?.updateEntry(for: nid) { entry in
                                guard entry.mostRecentValue != nodeValue else { return false }
                                entry.mostRecentValue = nodeValue
                                return true
                            }
                        }
                    } catch {
                        iterator?.logError(error)
                    }
                }
                self.metadataSubscription = AnyCancellable {
                    task.cancel()
                }
            }

            /// Requested data need for this node
            var dataNeed: DataNeed

            /// As we run computeValue we track the data need requested on its reruns
            /// We use this to compute the maximum data need requested on the previous run which becomes the new data need
            var largestNewDataNeed = Max<DataNeed>()

            /// AnyCancellable that cancels a task that updates mostRecentValue when metadataHandle emits updates for the node
            var metadataSubscription: AnyCancellable

            /// Cached value from the most recent update we received from the GraphRepository.
            var mostRecentValue: NodeValue<NoAugmentation>?

            var dataAvailabilitySubscription: AnyCancellable?

            /// Cached value from the most recent update we received from the GraphRepository.
            var mostRecentDataAvailability: DataAvailability?

            var dataSubscription: AnyCancellable?

            /// Cached value from the most recent update we received from the GraphRepository.
            var mostRecentData: Data?

            var effectiveDataNeed: DataNeed {
                max(dataNeed, largestNewDataNeed.max ?? dataNeed)
            }

            var isDataNeeded: Bool {
                switch mostRecentDataAvailability {
                case .availableLocally where effectiveDataNeed >= .wantDataIfLocal:
                    fallthrough
                case .availableRemotely where effectiveDataNeed >= .needDataEvenIfRemote:
                    return true
                case nil, .noData, .availableLocally, .availableRemotely:
                    return false
                }
            }

            mutating func setupDataSubscriptions() {
                guard effectiveDataNeed > .dataNotNeeded else {
                    dataAvailabilitySubscription = nil
                    dataSubscription = nil
                    return
                }

                if dataAvailabilitySubscription == nil {
                    let dataAvailabilityTask = Task { [nid, graphRepository, weak iterator] in
                        do {
                            iterator?.logDebug("setting up data availability updates for \(nid)")
                            let (cancellable, updates) = try await graphRepository.getDataAvailabilityUpdates(for: nid)
                            for await dataAvailability in updates {
                                await iterator?.updateEntry(for: nid) { entry in
                                    guard entry.mostRecentDataAvailability != dataAvailability else { return false }
                                    entry.mostRecentDataAvailability = dataAvailability
                                    entry.setupDataSubscriptions()
                                    return true
                                }
                            }
                        } catch {
                            iterator?.logError(error)
                        }
                    }
                    dataAvailabilitySubscription = AnyCancellable { dataAvailabilityTask.cancel() }
                }

                guard isDataNeeded else {
                    dataSubscription = nil
                    return
                }

                if dataSubscription == nil {
                    let dataTask = Task { [nid, graphRepository, weak iterator] in
                        do {
                            iterator?.logDebug("setting up data updates for \(nid)")
                            let (cancellable, updates) = try await graphRepository.getDataUpdates(for: nid)
                            for try await data in updates {
                                await iterator?.updateEntry(for: nid) { entry in
                                    guard entry.mostRecentData != data else { return false }
                                    entry.mostRecentData = data
                                    return true
                                }
                            }
                        } catch is DataDocument.DocumentClosedError {
                            iterator?.logInfo("data no longer available, metadata query should update entry appropriately so this doesn't recur")
                        } catch {
                            iterator?.logError(error)
                        }
                    }
                    dataSubscription = AnyCancellable { dataTask.cancel() }
                }
            }

            private func _completeValue(withDataNeed dataNeed: DataNeed) -> NodeValue<UntypedDataValue>? {
                guard let mostRecentValue else {
                    return nil
                }

                switch (dataNeed, mostRecentDataAvailability, mostRecentData) {
                case (.dataNotNeeded, _, _):
                    return mostRecentValue.withAugmentation(.dataNotChecked)
                case (.wantDataIfLocal, nil, _):
                    return nil
                case (.wantDataIfLocal, .noData, nil):
                    return mostRecentValue.withAugmentation(.dataIfLocal(.noData))
                case (.wantDataIfLocal, .availableLocally, .some(let data)):
                    return mostRecentValue.withAugmentation(.dataIfLocal(.localData(data)))
                case (.wantDataIfLocal, .availableRemotely, nil):
                    return mostRecentValue.withAugmentation(.dataIfLocal(.remoteDataExists))
                case (.needDataEvenIfRemote, nil, _):
                    return nil
                case (.needDataEvenIfRemote, .noData, nil):
                    return mostRecentValue.withAugmentation(.data(nil))
                case (.needDataEvenIfRemote, .availableLocally, .some(let data)):
                    return mostRecentValue.withAugmentation(.data(data))
                case (.needDataEvenIfRemote, .availableRemotely, nil):
                    return nil
                default:
                    iterator?.logWarn("for \(mostRecentValue.id) inconsistent combination of dataNeed=\(dataNeed), mostRecentDataAvailability=\(String(describing: mostRecentDataAvailability)), mostRecentData=\(String(describing: mostRecentData))")
                    // we may recover in the future
                    return nil
                }
            }

            var isValueComplete: Bool {
                _completeValue(withDataNeed: dataNeed) != nil
            }

            mutating func completeValue(withDataNeed dataNeed: DataNeed) -> NodeValue<UntypedDataValue>? {
                largestNewDataNeed.insert(dataNeed)
                setupDataSubscriptions()
                return _completeValue(withDataNeed: dataNeed)
            }

            consuming func resetDataNeedRequest(merge: (_ old: DataNeed, _ newMax: DataNeed) -> DataNeed) -> Self {
                guard let newDataNeed = largestNewDataNeed.max else {
                    return self
                }
                self.dataNeed = merge(self.dataNeed, newDataNeed)
                self.largestNewDataNeed.reset()
                setupDataSubscriptions()
                return self
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
                    entry.largestNewDataNeed.max != nil
                }
                .mapValues { entry in entry.resetDataNeedRequest(merge: { $1 }) }
        }

        /// If an entry is in this dictionary it is a dependency.
        private var dependencyEntries: [NID: DependencyEntry]

        private var dependencyEntriesAllComplete: Bool {
            dependencyEntries.allSatisfy { $0.value.isValueComplete }
        }

        private func trackedGetDependency(_ nid: NID, dataNeed: DataNeed) throws(FetchDependencyError) -> NodeValue<UntypedDataValue> {
            guard var entry = dependencyEntries[nid] else {
                logInfo("creating new dependency entry for \(nid)")
                dependencyEntries[nid] = DependencyEntry(
                    nid: nid,
                    dataNeed: dataNeed,
                    graphRepository: graphRepository,
                    iterator: self
                )
                logDebug("created new dependency entry for \(nid)")
                throw FetchDependencyError.cacheMiss
            }
            defer { dependencyEntries[nid] = entry }

            guard let value = entry.completeValue(withDataNeed: dataNeed) else {
                logDebug("don't have correct data level for \(nid)")
                throw FetchDependencyError.cacheMiss
            }
            logDebug("successfully fetched value for \(nid)")
            return value
        }

        var hasDependencyChangedSinceLastNextResult = true

        private func updateEntry(for nid: NID, change: (inout DependencyEntry) -> Bool) {
            guard var entry = dependencyEntries[nid] else {
                // maybe possible if considering concurrency shenanigans?
                logError("trying to update entry for \(nid) but we don't have a cache entry")
                return
            }
            defer { dependencyEntries[nid] = entry }
            if change(&entry) {
                markSomethingAsChanged()
            }
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
