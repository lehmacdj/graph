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

actor ProductionGraphRepository: GraphRepository {
    private let basePath: URL

    init(basePath: URL) {
        self.basePath = basePath
    }

    // MARK: GraphRepository

    func createNewNode() async throws -> NID {
        let nid = NID.random()
        let newMeta = NodeMeta(id: nid, incoming: [:], outgoing: [:])
        let data: Data = try JSONEncoder().encode(newMeta)
        let metaPath = basePath.appending(metaPathFor: nid)
        guard FileManager.default.createFile(atPath: metaPath.path, contents: data) else {
            throw CouldNotCreateFile(path: metaPath)
        }
        return nid
    }

    func updates<A: Augmentation>(
        for nid: NID,
        augmentedWith augmentation: A.Type
    ) -> any AsyncSequence<NodeValue<A>, any Error> {
        UpdatesSequence<A>(augmentedNid: nid, graphRepository: self)
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
        init(nid: NID, graphRepository: ProductionGraphRepository, metadataDocument: NodeMetadataDocument) {
            self.nid = nid
            self.graphRepository = graphRepository
            self.metadataDocument = metadataDocument
        }

        let nid: NID
        let graphRepository: ProductionGraphRepository
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
            metadataCache[nid]! = .closing()
            await metadataCache[nid]!.document!.close()
            guard case .closing(let waiters) = metadataCache[nid]! else {
                fatalError("only one closing operation may happen at a time")
            }
            waiters.forEach { $0.resume() }
            metadataCache.removeValue(forKey: nid)
        }
    }
}

/// AsyncSequence implementation for `updates`
private extension ProductionGraphRepository {
    /// Just a factory for `UpdatesIterator` which does all of the heavy lifting
    struct UpdatesSequence<A: Augmentation>: AsyncSequence {
        let augmentedNid: NID

        let graphRepository: ProductionGraphRepository

        func makeAsyncIterator() -> UpdatesIterator<A> {
            UpdatesIterator<A>(augmentedNid: augmentedNid, graphRepository: graphRepository)
        }
    }

    actor UpdatesIterator<A: Augmentation>: AsyncIteratorProtocol {
        init(augmentedNid: NID, graphRepository: ProductionGraphRepository) {
            self.augmentedNid = augmentedNid
            self.graphRepositroy = graphRepository
            self.dependencyEntries = [augmentedNid: DependencyEntry(
                mostRecentValue: nil,
                dependents: [augmentedNid]
            )]
            Task { [weak self] in
                await withDiscardingTaskGroup { [weak self] group in
                    while let self {
                        await ensureSubscriptionsExist { [weak self] nodeValues in
                            _ = group.addTaskUnlessCancelled { [weak self] in
                                for await nodeValue in nodeValues {
                                    await self?.updateValue(nodeValue)
                                }
                            }
                        }
                    }
                    group.cancelAll()
                }
            }
        }

        private let augmentedNid: NID

        private let graphRepositroy: ProductionGraphRepository

        private struct DependencyEntry {
            var mostRecentValue: NodeValue<Void>?

            /// Data need computed from the `mostRecentValue` (if that has ever been done)
            var mostRecentDataNeed: AugmentationDataNeed? = nil

            /// Dependencies computed from the `mostRecentValue`
            var mostRecentDependentNodes = Set<NID>()

            /// The `NID`s of `NodeValue`s which dependended on this entry when calling `Augmentation.computeDependencies`
            var dependents: Set<NID>

            /// MetadataHandle if we have already fetched it from the ProductionGraphRepository
            /// We use this to subscribe to updates
            var metadataHandle: MetadataHandle?

            var completeDependencyDictValue: NodeValue<AugmentationDataValue>? {
                guard let mostRecentValue, let mostRecentDataNeed else {
                    return nil
                }

                // TODO: process/handle data

                return mostRecentValue.withAugmentation(.dataNotChecked)
            }
        }

        /// If an entry is in this dictionary it is a dependency.
        private var dependencyEntries: [NID: DependencyEntry]

        private let someValueUpdated = AsyncChannel<Void>()

        /// If dependency resolution is complete the array of dependency values.
        private var completeDependencyDict: [NID: NodeValue<AugmentationDataValue>]? {
            if dependencyEntries.allSatisfy({ $0.value.completeDependencyDictValue != nil }) {
                dependencyEntries.mapValues { $0.completeDependencyDictValue! }
            } else {
                nil
            }
        }

        private func updateValue(_ newValue: NodeValue<Void>) {
            guard let entry = dependencyEntries[newValue.id] else {
                // maybe possible if considering concurrency shenanigans?
                logWarn("entry for node that is being updated doesn't exist")
                return
            }

            let (dataNeed, dependentNodes) = A.computeDependencies(forAugmenting: augmentedNid, from: newValue)
            for dependency in dependentNodes {
                ensureDependency(from: newValue.id, to: dependency)
            }

            do {
                let removedDependencies = entry.mostRecentDependentNodes.subtracting(dependentNodes)
                for dependency in removedDependencies {
                    removeDependency(from: newValue.id, to: dependency)
                }
            }

            guard var entry = dependencyEntries[newValue.id] else {
                // maybe possible if we removed a circular dependency?
                logWarn("entry for node that is being updated doesn't exist")
                return
            }
            entry.mostRecentDataNeed = dataNeed
            entry.mostRecentValue = newValue
            entry.mostRecentDependentNodes = dependentNodes
            dependencyEntries[newValue.id] = entry
            markSomethingAsUpdated()
        }

        private func ensureDependency(from nid: NID, to dependency: NID) {
            if var entry = dependencyEntries[dependency] {
                entry.dependents.insert(nid)
                dependencyEntries[dependency] = entry
            } else {
                dependencyEntries[dependency] = DependencyEntry(
                    mostRecentValue: nil,
                    dependents: [nid]
                )
            }
        }

        /// Update the dependents field by removing a dependency
        private func removeDependency(from nid: NID, to dependency: NID) {
            logDebug("removing dependency from \(nid) to \(dependency)")
            if var entry =
                dependencyEntries[dependency] {
                entry.dependents.remove(nid)
                if entry.dependents.isEmpty {
                    if let removedEntry = dependencyEntries.removeValue(forKey: dependency) {
                        removedEntry.mostRecentDependentNodes.forEach { removeDependency(from: dependency, to: $0)}
                    }
                } else {
                    dependencyEntries[dependency] = entry
                }
            }
        }

        private func ensureSubscriptionsExist(spawnNewListener: (any AsyncSequence<NodeValue<Void>, Never>) async -> Void) async {
            for (nid, var entry) in dependencyEntries where entry.metadataHandle == nil {
                do {
                    let handle = try await graphRepositroy.getMetadataHandle(for: nid)
                    entry.metadataHandle = handle
                    dependencyEntries[nid] = entry
                    await spawnNewListener(await handle.nodeValues)
                } catch {
                    logError(error.localizedDescription)
                }
            }
        }

        func next() async throws -> NodeValue<A>? {
            while completeDependencyDict == nil {
                await waitUntilSomethingUpdates()
            }
            guard let completeDependencyDict else {
                fatalError("loop above ensured non-nil")
            }

            let augmentation = A.computeAugmentation(for: augmentedNid, dependencies: completeDependencyDict)
            return completeDependencyDict[augmentedNid]!.withAugmentation(augmentation)
        }

        // MARK: way to wait on updates that affect completeDependencyDict

        private func markSomethingAsUpdated() {
            somethingUpdatedContinuation?.resume()
        }

        private func waitUntilSomethingUpdates() async {
            assert(somethingUpdatedContinuation == nil)
            await withCheckedContinuation { continuation in
                somethingUpdatedContinuation = continuation
            }
            somethingUpdatedContinuation = nil
        }

        private var somethingUpdatedContinuation: CheckedContinuation<Void, Never>?

    }
}
