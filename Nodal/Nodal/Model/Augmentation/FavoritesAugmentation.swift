//
//  FavoritesAugmentation.swift
//  Nodal
//
//  Created by Devin Lehmacher on 12/15/24.
//

import Foundation

/// Given a node `n`, looks for a transition "favorites" and includes the nodes that are prioritized.
/// TODO: abstract so that we don't write duplicate code for "worse" or other similar things because we definitely will want more of these
struct FavoritesAugmentation: Augmentation {
    let favorites: Set<NID>

    static func computeDependentNodes(forAugmenting id: NID, from node: NodeValue<Void>) -> Set<NID> {
        if id == node.id {
            return node.outgoing["favorites"] ?? []
        } else {
            return []
        }
    }

    static func computeAugmentation(for id: NID, dependencies: [NID : NodeValue<AugmentationDataValue>]) -> FavoritesAugmentation {
        let favoriteNodes = computeDependentNodes(forAugmenting: id, from: dependencies[id]!.withAugmentation(()))
        var result = Set<NID>()
        for nid in favoriteNodes {
            dependencies[nid]!.outgoing.values.forEach { result.formUnion($0) }
        }
        return FavoritesAugmentation(favorites: result)
    }
}
