//
//  Augmentation.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/4/24.
//

import Foundation

enum AugmentationDataNeed {
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

/// Protocol that defines types that can be used as augmentations
protocol Augmentation {
    /// Computes dependencies that the augmentation.
    ///
    /// This is called recursively on the returned dependencies until only `[]` is returned by leaf calls to `computeDependencies`.
    ///
    /// Note that this is only the dependencies required for computing the `Augmentation`. The node being augmented will always be a dependency of the an updates stream returned by a graph repository because it is needed for computing the `NodeValue` for that node itself.
    static func computeDependencies(forAugmenting id: NID, from node: NodeValue<Void>) -> (AugmentationDataNeed, Set<NID>)

    /// Convenience protocol method that can be implemented instead of the other `computeDependencies` overload; it assumes `dataNotNeeded` for all dependencies. There is a default implementation that uses computeDependencies to satisfy this protocol requirement for free.
    static func computeDependentNodes(forAugmenting id: NID, from node: NodeValue<Void>) -> Set<NID>

    /// Convenience protocol method that can be implemented to specify the behavior of computeDependencies when using the default implementation in terms of computeDependentNodes. There is a default implementation that returns `.dataNotNeeded` for all nodes.
    static func computeDataNeed(forAugmenting id: NID, from node: NodeValue<Void>) -> AugmentationDataNeed

    /// Computes the augmentation given an array of all of the resolved dependencies.
    ///
    /// Dependencies come with an async function that lets you fetch the data associated with any of the dependencies. Fetching the data within the body of `computeValue` establishes a dependency on that data. The data fetching closures should not be captured or called outside of `computeValue`, if it is behavior is undefined.
    static func computeAugmentation(for id: NID, dependencies: [NID: NodeValue<AugmentationDataValue>]) -> Self
}

/* I think we can probably implement computeDependencies + computeAugmentation in terms of this to provide a nicer API for implementing implementations; definitely a stretch goal though, not necessary short term for sure.
static func unfold(from node: NodeValue<DependencyRequester>, forAugmenting id: NID) async
 */

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
