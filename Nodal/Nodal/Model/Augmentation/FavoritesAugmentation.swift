//
//  FavoritesAugmentation.swift
//  Nodal
//
//  Created by Devin Lehmacher on 12/15/24.
//

import Foundation

/// Given a node `n`, looks for a transition "favorites" and includes the nodes that are prioritized.
/// TODO: abstract so that we don't write duplicate code for "worse" or other similar things because we definitely will want more of these
///
/// This mostly exists as an example of how to write a simple augmentation.
struct FavoritesAugmentation: Augmentation {
    let favorites: Set<NID>

    static func computeDependentNodes(forAugmenting id: NID, from node: NodeValue<Void>) -> Set<NID> {
        guard id == node.id else { return [] }
        return node.outgoing["favorites"] ?? []
    }

    static func computeAugmentation(for id: NID, dependencies: [NID : NodeValue<AugmentationDataValue>]) -> FavoritesAugmentation {
        let favoriteNodes = computeDependentNodes(forAugmenting: id, from: dependencies[id]!.withoutAugmentation())
        let favorites = favoriteNodes
            .flatMap { dependencies[$0]!.outgoing.values }
            .unioned()
        return FavoritesAugmentation(favorites: favorites)
    }
}
