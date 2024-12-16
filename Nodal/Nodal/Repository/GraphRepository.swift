//
//  GraphRepository.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/24.
//

protocol GraphRepository: Actor {
    /// Create a new node with a random NID returning the newly generated NID.
    func createNewNode() async throws -> NID

    /// Acquire a stream of updates for a NodeValue + augmentations computed from that node.
    ///
    /// There is a new element of the `AsyncSequence` whenever the data underlying the `NodeValue<A>` changes. The subscription stays active as long as the sequence is retained and the task is not cancelled.
    ///
    /// The sequence attempts to handle errors internally (e.g. by retrying) but when mitigations fail may throw an `Error` that describes why the subscription failed. This may be the first returned result if something is really broken.
    ///
    /// See ``Augmentation`` for more details on how augmentations work.
    func updates<A: Augmentation>(
        for nid: NID,
        augmentedWith augmentation: A.Type
    ) -> any AsyncSequence<NodeValue<A>, any Error>

    func deleteNode(withId id: NID) async throws

    func insertEdge(from: NID, to: NID, via transition: String) async throws
    func deleteEdge(from: NID, to: NID, via transition: String) async throws
}

extension GraphRepository {
    /// Default implementation of ``updates(for:augmentedWith:)`` that provides a default implementation of augmentation to use ``NoAugmentation``.
    ///
    /// Note that you must stll implement ``updates(for:augmentedWith:)`` as otherwise this implementation will recurse infinitely.
    func updates<A: Augmentation>(
        for nid: NID,
        augmentedWith augmentation: A.Type = NoAugmentation.self
    ) -> any AsyncSequence<NodeValue<A>, any Error> {
        updates<A>(for: nid, augmentedWith: augmentation)
    }
}
