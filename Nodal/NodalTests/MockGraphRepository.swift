//
//  MockGraphRepository.swift
//  NodalTests
//
//  Created by Devin Lehmacher on 9/10/26.
//

@testable import Nodal
import Foundation

/// A ``GraphRepository`` that serves a single node with test-controlled data.
///
/// Every subscription created by ``updates(computeValue:)`` recomputes its value
/// whenever ``emit()`` is called, and records the ``DataNeed`` that the compute
/// closure asked for so tests can tell `.wantDataIfLocal` subscriptions apart
/// from `.needDataEvenIfRemote` ones.
actor MockGraphRepository: GraphRepository {
    /// What a `.wantDataIfLocal` fetch sees.
    private var localData: LocalDataValue

    /// What a `.needDataEvenIfRemote` fetch sees, i.e. what downloading yields.
    private var downloadedData: DataWithURL?

    init(
        localData: LocalDataValue = .noData,
        downloadedData: DataWithURL? = nil
    ) {
        self.localData = localData
        self.downloadedData = downloadedData
    }

    /// Data needs requested by compute closures, oldest first.
    private(set) var requestedDataNeeds: [DataNeed] = []

    var lastRequestedDataNeed: DataNeed? { requestedDataNeeds.last }

    func setLocalData(_ localData: LocalDataValue) {
        self.localData = localData
    }

    func setDownloadedData(_ downloadedData: DataWithURL?) {
        self.downloadedData = downloadedData
    }

    private var subscribers: [(DependencyManager) -> Void] = []

    func updates<T: Sendable>(
        computeValue: @escaping ComputeValueClosure<T>
    ) -> sending any AsyncSequence<T, Error> {
        let (stream, continuation) = AsyncThrowingStream<T, Error>.makeStream()
        let recompute: (DependencyManager) -> Void = { dependencyManager in
            // a compute closure that can't satisfy its dependencies just doesn't
            // produce a value yet, which is what the real repository does too
            if let value = try? computeValue(dependencyManager) {
                continuation.yield(value)
            }
        }
        subscribers.append(recompute)
        recompute(makeDependencyManager())
        return stream
    }

    /// Push the current data value to every live subscription.
    func emit() {
        let dependencyManager = makeDependencyManager()
        for recompute in subscribers {
            recompute(dependencyManager)
        }
    }

    private func makeDependencyManager() -> DependencyManager {
        RecordingDependencyManager(
            localData: localData,
            downloadedData: downloadedData
        ) { [self] dataNeed in
            requestedDataNeeds.append(dataNeed)
        }
    }

    // MARK: unused by these tests

    func createNewNode() async throws -> NID { NID(fake: 0) }
    nonisolated func getHypotheticalDataPath(for nid: NID) -> URL {
        URL(filePath: "/dev/null")
    }
    func deleteNode(withId id: NID) async throws {}
    func insertEdge(from: NID, to: NID, via transition: String) async throws {}
    func deleteEdge(from: NID, to: NID, via transition: String) async throws {}
}

/// Serves an empty node for every NID, with the mock's data as its data.
private struct RecordingDependencyManager: DependencyManager {
    let localData: LocalDataValue
    let downloadedData: DataWithURL?
    let record: (DataNeed) -> Void

    func fetch<D: TypedDataNeed>(
        nid: NID,
        dataNeed: D
    ) throws(FetchDependencyError) -> Node<D.Value> {
        record(dataNeed.untyped)
        guard let value = dataNeed.coerceValue(coerce(dataNeed.untyped)) else {
            throw .cacheMiss
        }
        return node(nid: nid, augmentation: value)
    }

    func fetch(
        nid: NID,
        untypedDataNeed: DataNeed
    ) throws(FetchDependencyError) -> Node<UntypedDataValue> {
        record(untypedDataNeed)
        return node(nid: nid, augmentation: coerce(untypedDataNeed))
    }

    /// Narrow the mock's data down to what `dataNeed` actually asked for.
    private func coerce(_ dataNeed: DataNeed) -> UntypedDataValue {
        switch dataNeed {
        case .dataNotNeeded:
            .dataNotChecked
        case .needsAvailabilityOnly:
            .availabilityOnly(localData.availability)
        case .wantDataIfLocal:
            .dataIfLocal(localData)
        case .needDataEvenIfRemote:
            .data(downloadedData)
        }
    }

    private func node<A>(nid: NID, augmentation: A) -> Node<A> {
        Node(id: nid, outgoing: [:], incoming: [:], augmentation: augmentation)
    }
}

private extension LocalDataValue {
    var availability: DataAvailability {
        switch self {
        case .noData: .noData
        case .localData(let dataWithURL): .availableLocally(dataWithURL.url)
        case .remoteDataExists(let url): .availableRemotely(url)
        }
    }
}
