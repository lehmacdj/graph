//
//  NodeValue.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/4/24.
//

@dynamicMemberLookup
struct NodeValue<Augmentation: Sendable>: Identifiable, Sendable {
    let id: NID
    let outgoing: [String:Set<NID>]
    let incoming: [String:Set<NID>]

    /// Generally prefer using fields of the augmentation direction. They are exposed via @dynamicMemberLookup
    let augmentation: Augmentation

    subscript<T>(dynamicMember member: KeyPath<Augmentation, T>) -> T {
        augmentation[keyPath: member]
    }
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

    func mapAugmentation<NewAugmentation>(_ transform: (Augmentation) throws -> NewAugmentation) rethrows -> NodeValue<NewAugmentation> {
        NodeValue<NewAugmentation>(
            id: id,
            outgoing: outgoing,
            incoming: incoming,
            augmentation: try transform(augmentation)
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
