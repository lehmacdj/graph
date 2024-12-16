//
//  NoAugmentation.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/4/24.
//

import Foundation

/// Trivial augmentation that includes no data
struct NoAugmentation {}

extension NoAugmentation: Augmentation {
    static func computeDependentNodes(forAugmenting id: NID, from node: NodeValue<Void>) -> Set<NID> {
        []
    }

    static func computeAugmentation(for id: NID, dependencies: [NID: NodeValue<AugmentationDataValue>]) -> NoAugmentation {
        return .init()
    }
}
