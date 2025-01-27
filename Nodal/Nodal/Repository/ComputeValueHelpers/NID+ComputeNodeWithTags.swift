//
//  NID+ComputeNodeWithTags.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/26/25.
//

import Algorithms
import Foundation

struct TagsAugmentation {
    let tags: Set<String>
}

extension NID {
    private func parseAndFetch(
        node: Node<NoAugmentation>,
        timeBuilder: GraphTimestampBuilder,
        parse: (GraphTimestampBuilder, String) -> GraphTimestampBuilder?,
        dependencyManager: DependencyManager
    ) -> [(GraphTimestampBuilder, Node<NoAugmentation>)?] {
        node.incoming.flatMap { (transition, nids) in
            // Attempt to parse the transition to get a new builder
            guard let updatedBuilder = parse(timeBuilder, transition) else {
                return [(GraphTimestampBuilder, Node<NoAugmentation>)?]()
            }
            // For each subsequent NID, try fetching the node and pair it with the updated builder
            return nids
                .map { try? dependencyManager.fetch(nid: $0, dataNeed: .dataNotNeeded) }
                .map { maybeNodeValue in
                    maybeNodeValue.map { (updatedBuilder, $0) }
                }
        }
    }


    func computeNodeWithTags(dependencyManager: DependencyManager) throws(FetchDependencyError) -> Node<TagsAugmentation> {
        let node = try dependencyManager.fetch(nid: self, dataNeed: .dataNotNeeded)
        let tagsNode = try dependencyManager.fetch(nid: NID.tags, dataNeed: .dataNotNeeded)
        
        let incomingNids = node.incoming.flatMap(\.value).to(Set.init)
        let tags = tagsNode
            .outgoingTransitions
            .filter { transition in incomingNids.contains(transition.nid) }
            // if there are two transitions with the same name but different nids this will collapse them
            .map { transition in transition.transition }
            .to(Set.init)

        return node.withAugmentation(TagsAugmentation(tags: tags))
    }
}
