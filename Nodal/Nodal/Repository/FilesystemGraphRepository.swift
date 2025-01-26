//
//  ProductionGraphRepository.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/4/24.
//

import Foundation
@preconcurrency import Combine
import UIKit
import AsyncAlgorithms

actor FilesystemGraphRepository: GraphRepository {
    private let basePath: URL

    init(basePath: URL) async throws {
        self.basePath = basePath
        metadataCache = ConcurrentCache(
            create: { try await NodeMetadataDocument(metaURL: basePath.appending(metaPathFor: $0)) },
            destroy: { (nid, document) in
                if await !document.close() {
                    logError("failed to save while closing metadata document for \(nid)")
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
                if await !document.close() {
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
        let newMeta = Node(id: nid, outgoing: [:], incoming: [:])
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

    func updates<T: Sendable>(computeValue: @escaping ComputeValueClosure<T>) -> any SendableAsyncSequence<T, Error> {
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

    private func getMetadataUpdates(for nid: NID) async throws -> (ConcurrentCache<NID, NodeMetadataDocument>.Handle, any SendableAsyncSequence<Node<NoAugmentation>, Never>) {
        let (cacheEntry, metadataDocument) = try await metadataCache.getOrCreate(nid)
        let sequence = await metadataDocument
            .metaPublisher
            .values
        return (cacheEntry, sequence)
    }

    private var dataAvailabilityCache: ConcurrentCache<NID, FileAvailabilityObserver>

    private func getDataAvailabilityUpdates(for nid: NID) async throws -> (ConcurrentCache<NID, FileAvailabilityObserver>.Handle, any SendableAsyncSequence<DataAvailability, Never>) {
        let (cacheEntry, observer) = try await dataAvailabilityCache.getOrCreate(nid)
        let updates = await observer.updates
        return (cacheEntry, updates)
    }

    private let dataDocumentCache: ConcurrentCache<NID, DataDocument>

    private func getDataUpdates(for nid: NID) async throws -> (ConcurrentCache<NID, DataDocument>.Handle, any SendableAsyncSequence<Data, Error>) {
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
    struct UpdatesSequence<T: Sendable>: AsyncSequence, Sendable {
        let graphRepository: FilesystemGraphRepository

        let computeValue: ComputeValueClosure<T>

        init(graphRepository: FilesystemGraphRepository, computeValue: @escaping ComputeValueClosure<T>) {
            self.graphRepository = graphRepository
            self.computeValue = computeValue
        }

        func makeAsyncIterator() -> UpdatesIterator<T> {
            UpdatesIterator<T>(graphRepository: graphRepository, computeValue: computeValue)
        }
    }

    actor UpdatesIterator<T: Sendable>: AsyncIteratorProtocol {
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
                    do {
                        try await waitUntilSomethingChanges()
                    } catch {
                        // task was cancelled
                        return nil
                    }
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
            let untypedGet: (NID, DataNeed) throws(FetchDependencyError) -> Node<UntypedDataValue>

            func fetch<D>(nid: NID, dataNeed: D) throws(FetchDependencyError) -> Node<D.Value> where D : TypedDataNeed {
                let nodeValue = try untypedGet(nid, dataNeed.untyped)
                guard let typedAugmentation = dataNeed.coerceValue(nodeValue.data) else {
                    fatalError("type mismatch: wrong augmentation type for data need \(dataNeed.untyped)")
                }
                return nodeValue.withAugmentation(typedAugmentation)
            }

            func fetch(nid: NID, untypedDataNeed: DataNeed) throws(FetchDependencyError) -> Node<UntypedDataValue> {
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
                self.metadataSubscription = Self.setupMetadataSubscription(nid: nid, graphRepository: graphRepository, iterator: iterator)
            }

            /// Requested data need for this node
            var dataNeed: DataNeed

            /// As we run computeValue we track the data need requested on its reruns
            /// We use this to compute the maximum data need requested on the previous run which becomes the new data need
            var largestNewDataNeed = Max<DataNeed>()

            /// AnyCancellable that cancels a task that updates mostRecentValue when metadataHandle emits updates for the node
            var metadataSubscription: AnyCancellable

            /// Cached value from the most recent update we received from the GraphRepository.
            var mostRecentValue: Node<NoAugmentation>?

            var dataAvailabilitySubscription: AnyCancellable?

            /// Cached value from the most recent update we received from the GraphRepository.
            var mostRecentDataAvailability: DataAvailability?

            var dataSubscription: AnyCancellable?

            /// Cached value from the most recent update we received from the GraphRepository.
            var mostRecentData: Data??

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

            static func setupMetadataSubscription(
                nid: NID,
                graphRepository: FilesystemGraphRepository,
                iterator: UpdatesIterator
            ) -> AnyCancellable {
                let task = Task { [nid, graphRepository, weak iterator] in
                    do {
                        let (cancellable, updates) = try await graphRepository.getMetadataUpdates(for: nid)
                        for await nodeValue in updates {
                            await iterator?.updateEntry(for: nid) { entry in
                                guard entry.mostRecentValue != nodeValue else { return false }
                                entry.mostRecentValue = nodeValue
                                return true
                            }
                        }
                        _ = cancellable // needs to live until here so that updates doesn't get deallocated
                        logDebug("metadata subscription for \(nid) finishing")
                    } catch {
                        logError(error)
                    }
                }
                return AnyCancellable { task.cancel() }
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
                            let (cancellable, updates) = try await graphRepository.getDataAvailabilityUpdates(for: nid)
                            for await dataAvailability in updates {
                                await iterator?.updateEntry(for: nid) { entry in
                                    guard entry.mostRecentDataAvailability != dataAvailability else { return false }
                                    entry.mostRecentDataAvailability = dataAvailability
                                    entry.setupDataSubscriptions()
                                    return true
                                }
                            }
                            _ = cancellable // needs to live until here so that updates doesn't get deallocated
                            logDebug("data availability subscription for \(nid) finishing")
                        } catch{
                            logError(error)
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
                            let (cancellable, updates) = try await graphRepository.getDataUpdates(for: nid)
                            for try await data in updates {
                                await iterator?.updateEntry(for: nid) { entry in
                                    guard entry.mostRecentData != data else { return false }
                                    entry.mostRecentData = .some(.some(data))
                                    return true
                                }
                            }
                            _ = cancellable // needs to live until here so that updates doesn't get deallocated
                            logDebug("data subscription for \(nid) finishing")
                        } catch is DataDocument.DocumentClosedError {
                            logInfo("data for \(nid) no longer available, data availability update should prevent recurrence")
                        } catch {
                            logError(error)
                        }
                    }
                    dataSubscription = AnyCancellable { dataTask.cancel() }
                }
            }

            enum IncompleteValueReason: Error {
                case missingMetadata
                case needDataAvailability
                case needData
                case inconsistentDataNeedAvailabilityAndData
            }

            private func _completeValue(withDataNeed dataNeed: DataNeed) throws -> Node<UntypedDataValue> {
                guard let mostRecentValue else {
                    throw IncompleteValueReason.missingMetadata
                }

                switch (dataNeed, mostRecentDataAvailability, mostRecentData) {
                case (.dataNotNeeded, _, _):
                    return mostRecentValue.withAugmentation(.dataNotChecked)
                case (.wantDataIfLocal, .none, _):
                    throw IncompleteValueReason.needDataAvailability
                case (.wantDataIfLocal, .noData, .none):
                    return mostRecentValue.withAugmentation(.dataIfLocal(.noData))
                case (.wantDataIfLocal, .availableLocally, .none):
                    throw IncompleteValueReason.needData
                case (.wantDataIfLocal, .availableLocally, .some(.some(let data))):
                    return mostRecentValue.withAugmentation(.dataIfLocal(.localData(data)))
                case (.wantDataIfLocal, .availableRemotely, .none):
                    return mostRecentValue.withAugmentation(.dataIfLocal(.remoteDataExists))
                case (.needDataEvenIfRemote, .none, _):
                    throw IncompleteValueReason.needDataAvailability
                case (.needDataEvenIfRemote, .noData, .none):
                    return mostRecentValue.withAugmentation(.data(nil))
                case (.needDataEvenIfRemote, .availableLocally, .none):
                    throw IncompleteValueReason.needData
                case (.needDataEvenIfRemote, .availableLocally, .some(.some(let data))):
                    return mostRecentValue.withAugmentation(.data(data))
                case (.needDataEvenIfRemote, .availableRemotely, nil):
                    throw IncompleteValueReason.needData
                case (.needDataEvenIfRemote, .availableRemotely, .some(.some(let data))):
                    // because of the refresh interval for FileAvailabilityObserver this state is
                    // fairly common just after requesting data from the server
                    // it is better to treat this as the data existing
                    return mostRecentValue.withAugmentation(.data(data))
                default:
                    logWarn("inconsistent combination of dataNeed=\(dataNeed), mostRecentDataAvailability=\(mostRecentDataAvailability.compactDescription), mostRecentData=\(mostRecentData.compactDescription))")
                    // we may recover in the future
                    throw IncompleteValueReason.inconsistentDataNeedAvailabilityAndData
                }
            }

            var isValueComplete: Bool {
                Result { try _completeValue(withDataNeed: dataNeed) }
                    .isSuccess
            }

            mutating func completeValue(withDataNeed dataNeed: DataNeed) throws -> Node<UntypedDataValue> {
                largestNewDataNeed.insert(dataNeed)
                setupDataSubscriptions()
                return try _completeValue(withDataNeed: dataNeed)
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

        private func trackedGetDependency(_ nid: NID, dataNeed: DataNeed) throws(FetchDependencyError) -> Node<UntypedDataValue> {
            guard var entry = dependencyEntries[nid] else {
                logDebug("creating new dependency entry for \(nid)")
                dependencyEntries[nid] = DependencyEntry(
                    nid: nid,
                    dataNeed: dataNeed,
                    graphRepository: graphRepository,
                    iterator: self
                )
                throw FetchDependencyError.cacheMiss
            }
            defer { dependencyEntries[nid] = entry }

            do {
                return try entry.completeValue(withDataNeed: dataNeed)
            } catch {
                logDebug("failed to get complete value for \(nid) for reason \(error)")
                throw FetchDependencyError.cacheMiss
            }
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
            resumeSomethingUpdatedContinuation()
        }

        private var isWaitingUntilSomethingChanges = false

        /// Wait until something changes
        private func waitUntilSomethingChanges() async throws(CancellationError) {
            assert(somethingUpdatedContinuation == nil)
            assert(!isWaitingUntilSomethingChanges)
            assert(!dependencyEntriesAllComplete || !hasDependencyChangedSinceLastNextResult)
            isWaitingUntilSomethingChanges = true
            defer { isWaitingUntilSomethingChanges = false }
            await withTaskCancellationHandler {
                await withCheckedContinuation { continuation in
                    somethingUpdatedContinuation = continuation
                }
            } onCancel: {
                Task { await resumeSomethingUpdatedContinuation() }
            }
            do {
                try Task.checkCancellation()
            } catch is CancellationError {
                throw CancellationError()
            } catch {
                logError(error)
            }
        }

        private var somethingUpdatedContinuation: CheckedContinuation<Void, Never>?

        private func resumeSomethingUpdatedContinuation() {
            if let continuation = somethingUpdatedContinuation {
                continuation.resume()
                somethingUpdatedContinuation = nil
            } else {
                logDebug("nil continuation when trying to resume continuation")
            }
        }
    }
}
