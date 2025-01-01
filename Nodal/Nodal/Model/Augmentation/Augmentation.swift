//
//  Augmentation.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/4/24.
//

import Foundation

enum AugmentationDataNeed: Comparable {
    case dataNotNeeded
    case wantDataIfLocal
    case needDataEvenIfRemote
}

enum LocalDataValue {
    case noData
    case localData(Data)
    case remoteDataExists
}

enum AugmentationDataValue {
    case dataNotChecked
    case dataIfLocal(LocalDataValue)
    case data(Data?)
}

// TODO: I need something fancier; for stuff like TimestampAugmentation we need a little bit more context while deciding what the dependencies are.
// - We could just collect an absurd number of dependencies by pattern matching at each step for the date pattern and then traverse after the fact in computeAugmentation, but this kind of hack is exactly what I was hoping to avoid by introducing a complicated augmentation mechanism
// - I'm thinking we probably want to go ahead and expose the graph structure to computeDependencies.
// - Maybe switching to some kind of "unfold" paradigm makes sense too, that's what my original API was modeled after, so maybe going whole hog for the comonad would make this neater?
//   - `func computeDependencies(unfolding: (NodeValue<FetchedDependencies>) -> Dependencies)`
//   - something like the above until reading a fixpoint?
//   - main concerns are:
//     - computational efficiency: because you have to figure out from scratch which things to check and that could start to get expensive if something has a very large amount of dependencies
//     - redundant computeDependencies implementations
//     - implementation complexity: it starts to get a lot harder to keep track of which dependencies can be trimmed/removed if stuff changes, fairly significant changes to existing paradigm too
//   - such a pattern would also allow "discovering" non-local dependencies, e.g. discovering that the date is recent could trigger fetching additional information as well, or dependencies discovered via data

/// Protocol that defines types that can be used as augmentations
protocol Augmentation {
    /// Computes dependencies that the augmentation.
    ///
    /// This is called recursively on the returned dependencies until only `[]` is returned by leaf calls to `computeDependencies`.
    ///
    /// Note that this is only the dependencies required for computing the `Augmentation`. The node being augmented will always be a dependency of the an updates stream returned by a graph repository because it is needed for computing the `NodeValue` for that node itself.
    static func computeDependencies(forAugmenting id: NID, from node: NodeValue<Void>) -> (AugmentationDataNeed, Set<NID>)

    /// Convenience protocol method that can be implemented instead of the other `computeDependencies` overload; it assumes `dataNotNeeded` for all dependencies.
    static func computeDependentNodes(forAugmenting id: NID, from node: NodeValue<Void>) -> Set<NID>

    /// Convenience protocol method that can be implemented to specify the behavior of computeDependencies when using the default implementation in terms of computeDependentNodes. There is a default implementation that returns `.dataNotNeeded` for all nodes.
    static func computeDataNeed(forAugmenting id: NID, from node: NodeValue<Void>) -> AugmentationDataNeed

    /// Computes the augmentation given an array of all of the resolved dependencies.
    ///
    /// Dependencies come with an async function that lets you fetch the data associated with any of the dependencies. Fetching the data within the body of `computeValue` establishes a dependency on that data. The data fetching closures should not be captured or called outside of `computeValue`, if it is behavior is undefined.
    static func computeAugmentation(for id: NID, dependencies: [NID: NodeValue<AugmentationDataValue>]) -> Self
}

struct Dependencies {
    let nodes: [NID: NodeValue<Void>]
    let data: AugmentationDataValue
}

extension Augmentation {
    // a stronger compute dependencies that makes it possible to implement Timestamp in a not terrible way?
    // how can we make the graph structure more usable by the person implementing the protocol method so that we don't have to make as many assumptions while writing augmentation implementations?
    static func computeDependencies(
        unfolding: NodeValue<Dependencies>
    ) -> // this would be all dependencies, we would return the burden of removing unnecessary dependencies to the Augmentation implementor
        [NID: AugmentationDataNeed] {
        [:]
    }
}

extension Augmentation {
    static func computeDependentNodes(forAugmenting id: NID, from node: NodeValue<Void>) -> Set<NID> {
        computeDependencies(forAugmenting: id, from: node).1
    }

    static func computeDataNeed(forAugmenting _: NID, from _: NodeValue<Void>) -> AugmentationDataNeed {
        return .dataNotNeeded
    }

    static func computeDependencies(forAugmenting id: NID, from node: NodeValue<Void>) -> (AugmentationDataNeed, Set<NID>) {
        let dataNeed = computeDataNeed(forAugmenting: id, from: node)
        let dependencies = computeDependentNodes(forAugmenting: id, from: node)
        return (dataNeed, dependencies)
    }
}
