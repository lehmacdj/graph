//
//  Augmentation.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/4/24.
//

import Foundation

/// Protocol that defines types that can be used as augmentations
protocol Augmentation {
    /// Computes dependencies that the augmentation. This is called recursively on the returned dependencies until only `[]` is returned by leaf calls to `computeDependencies`
    static func computeDependencies(forAugmenting id: NID, from node: NodeValue<Void>) -> [NID]

    /// Computes the augmentation given an array of all of the resolved dependencies. Dependencies come with an async function that lets you fetch the data associated with any of the dependencies.
    static func computeValue(forAugmenting id: NID, dependencies: [NID: NodeValue<() async -> Data>]) async -> Self
}
