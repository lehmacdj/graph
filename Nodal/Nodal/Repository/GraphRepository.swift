//
//  GraphRepository.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/24.
//


/// Error used by ``GraphRepository.updates``
enum FetchDependencyError: Error {
    case cacheMiss
    /// This error isn't thrown by the closure passed to computeValue ever, but may be thrown by the computeValue closure to indicate that it doesn't have enough information to compute its value
    /// We could consider adding more info to this error or separating it from cacheMiss to make the types a bit stronger, this mostly exists so that you never have to write `throw .cacheMiss` in the implementation of a `computeValue` closure.
    case missingDependencies
}

typealias FetchDependencyClosure = (NID, AugmentationDataNeed) throws(FetchDependencyError) -> NodeValue<AugmentationDataValue>

typealias ComputeValueClosure<T> = (FetchDependencyClosure) throws(FetchDependencyError) -> T

protocol GraphRepository: Actor {
    /// Create a new node with a random NID returning the newly generated NID.
    func createNewNode() async throws -> NID

    /// Acquire a stream of updates for a value computed by computeValue.
    ///
    /// `computeValue` is repeatedly called to discover dependencies.
    /// - Dependencies are updated every time a value is successfully returned by the `computeValue` closure.
    /// - You may catch thrown `FetchDependencyError`s. This can be useful to declare a dependency on several nodes at the same time instead of waiting for nodes to be fetched sequentially.
    /// - It is `computeValue`'s implementor's responsibility to make sure that the dependencies are stable enough to capture all data from the graph that is actually depended on. If you only use values returned by the closure and don't store them externally to the closure, this should be achieved for free.
    ///
    /// The sequence attempts to handle errors internally (e.g. by retrying) but when mitigations fail may throw an `Error` that describes why the subscription failed. This may be the first returned result if something is really broken.
    func updates<T>(computeValue: @escaping ComputeValueClosure<T>) -> any AsyncSequence<T, any Error>

    func deleteNode(withId id: NID) async throws

    func insertEdge(from: NID, to: NID, via transition: String) async throws
    func deleteEdge(from: NID, to: NID, via transition: String) async throws
}
