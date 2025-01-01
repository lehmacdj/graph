//
//  NodeValue.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/4/24.
//

struct NodeValue<Augmentation>: Identifiable {
    let id: NID
    let outgoing: [String:Set<NID>]
    let incoming: [String:Set<NID>]

    /// I don't think it is worth prioritizing because it is very boilerplate heavy but it is possible to support typed @dynamicMemberLookup via an approach similar to this:
    /// https://developer.apple.com/documentation/foundation/attributedstringkey
    let augmentation: Augmentation
}

extension NodeValue {
    init(from metadata: NodeMeta, augmentation: Augmentation) {
        self.init(
            id: metadata.id,
            outgoing: metadata.outgoing,
            incoming: metadata.incoming,
            augmentation: augmentation
        )
    }
}

extension NodeValue<Void> {
    init(from metadata: NodeMeta) {
        self.init(from: metadata, augmentation: ())
    }
}

extension NodeValue {
    func withAugmentation<NewAugmentation>(_ newAugmentation: NewAugmentation) -> NodeValue<NewAugmentation> {
        NodeValue<NewAugmentation>(
            id: id,
            outgoing: outgoing,
            incoming: incoming,
            augmentation: newAugmentation
        )
    }

    func withoutAugmentation() -> NodeValue<Void> {
        self.withAugmentation(())
    }
}

extension NodeValue {
    var outgoingTransitions: [NodeTransition] {
        outgoing.flatMap { (transition, destinations) in
            destinations.map { NodeTransition(transition: transition, nid: $0) }
        }
    }

    var incomingTransitions: [NodeTransition] {
        incoming.flatMap { (transition, sources) in
            sources.map { NodeTransition(transition: transition, nid: $0) }
        }
    }
}
