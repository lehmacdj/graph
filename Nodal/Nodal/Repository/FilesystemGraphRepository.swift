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

    // MARK: Subscription management

    enum NodeCacheEntry<Document: UIDocument> {
        /// If there are concurrent attempts to open a particular entry all but the first may have to wait for the first attempt to complete
        case opening(waiters: [CheckedContinuation<Void, Never>] = [])
        case live(referenceCount: Int, document: Document)
        /// If there are concurrent attempts to open a particular entry all but the first may have to wait for the first attempt to complete
        case closing(waiters: [CheckedContinuation<Void, Never>] = [])

        var document: Document? {
            switch self {
            case .live(_, let document): document
            case .opening, .closing: nil
            }
        }

        var referenceCount: Int? {
            switch self {
            case .live(let referenceCount, _): referenceCount
            case .closing, .opening: nil
            }
        }

        mutating func decrementReferenceCount() {
            guard case .live(let referenceCount, let document) = self else {
                fatalError("can't change reference count if not live")
            }
            self = .live(referenceCount: referenceCount - 1, document: document)
        }

        mutating func incrementReferenceCount() {
            guard case .live(let referenceCount, let document) = self else {
                fatalError("can't change reference count if not live")
            }
            self = .live(referenceCount: referenceCount + 1, document: document)
        }

        mutating func addWaiter(_ continuation: CheckedContinuation<Void, Never>) {
            switch self {
            case .live:
                fatalError("invalid to add a waiter when live")
            case .opening(let waiters):
                self = .opening(waiters: waiters + [continuation])
            case .closing(let waiters):
                self = .closing(waiters: waiters + [continuation])
            }
        }
    }

    class MetadataHandle {
        init(nid: NID, graphRepository: FilesystemGraphRepository, metadataDocument: NodeMetadataDocument) {
            self.nid = nid
            self.graphRepository = graphRepository
            self.metadataDocument = metadataDocument
        }

        let nid: NID
        let graphRepository: FilesystemGraphRepository
        let metadataDocument: NodeMetadataDocument

        deinit {
            Task { [graphRepository, nid] in
                await graphRepository.removeMetadataReference(for: nid)
            }
        }

        var nodeValues: any AsyncSequence<NodeValue<Void>, Never> {
            get async {
                // I have some concerns about what happens if the metadata document is closed but we're still subscribed to this sequence, but it should be pretty obvious something is broken if that breaks something
                let publisher = await metadataDocument.metaPublisher
                return AsyncStream { continuation in
                    let cancellable = publisher
                        .sink { result in
                            switch result {
                            case .finished:
                                continuation.finish()
                            }
                        } receiveValue: { value in
                            continuation.yield(NodeValue(from: value))
                        }
                    continuation.onTermination = { _ in
                        cancellable.cancel()
                    }
                }
            }
        }
    }

    private var metadataCache = [NID: NodeCacheEntry<NodeMetadataDocument>]()

    private func getMetadataHandle(for nid: NID) async throws -> MetadataHandle {
        // ensure we have a live cache entry
        while true {
            if case .live = metadataCache[nid] {
                // leave the loop, then in the guard we will have a live cache entry
                break
            } else if var cacheEntry = metadataCache[nid] {
                // wait for the closing or opening operation to complete then try again
                await withCheckedContinuation { continuation in
                    cacheEntry.addWaiter(continuation)
                    metadataCache[nid] = cacheEntry
                }
            } else {
                // initialize a new cache entry
                metadataCache[nid] = .opening()
                do {
                    let document = try await NodeMetadataDocument(metaURL: basePath.appending(metaPathFor: nid))
                    guard case .opening(let waiters) = metadataCache[nid] else {
                        fatalError("only one opening operation can be active at a time")
                    }
                    waiters.forEach { $0.resume() }
                    metadataCache[nid]! = .live(referenceCount: 0, document: document)
                    // we probably could break/return here, but the logic is clearer if there
                    // is only a single point where it is possible to break out of the loop
                } catch {
                    metadataCache.removeValue(forKey: nid)
                    throw error
                }
            }
        }

        // safe because we checked above without suspend points in between
        metadataCache[nid]!.incrementReferenceCount()

        return MetadataHandle(nid: nid, graphRepository: self, metadataDocument: metadataCache[nid]!.document!)
    }

    private func removeMetadataReference(for nid: NID) async {
        guard case .live = metadataCache[nid] else {
            // we only return a MetadataHandle when .live and only transition out of that
            // state in this method
            fatalError("cache was manipulated improperly")
        }

        metadataCache[nid]!.decrementReferenceCount()

        if metadataCache[nid]!.referenceCount! == 0 {
            let document = metadataCache[nid]!.document!
            metadataCache[nid]! = .closing()
            await document.close()
            guard case .closing(let waiters) = metadataCache[nid]! else {
                fatalError("only one closing operation may happen at a time")
            }
            waiters.forEach { $0.resume() }
            metadataCache.removeValue(forKey: nid)
        }
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
                        let handle = try await graphRepository.getMetadataHandle(for: nid)
                        for await nodeValue in await handle.nodeValues {
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

            entry.mostRecentValue = newValue
            dependencyEntries[newValue.id] = entry
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
