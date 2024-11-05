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
    static func computeDependencies(forAugmenting id: NID, from node: NodeValue<Void>) -> [NID] {
        []
    }

    static func computeValue(forAugmenting id: NID, dependencies: [NID: NodeValue<() async -> Data>]) async -> NoAugmentation {
        return .init()
    }
}
