//
//  NodeValue.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/4/24.
//

@dynamicMemberLookup
struct Node<Augmentation: Sendable>: Identifiable, Sendable {
    let id: NID
    let outgoing: [String:Set<NID>]
    let incoming: [String:Set<NID>]

    /// Generally prefer using fields of the augmentation direction. They are exposed via @dynamicMemberLookup
    let augmentation: Augmentation

    subscript<T>(dynamicMember member: KeyPath<Augmentation, T>) -> T {
        augmentation[keyPath: member]
    }
}

extension Node: Equatable where Augmentation: Equatable {}

extension Node {
    func withAugmentation<NewAugmentation>(_ newAugmentation: NewAugmentation) -> Node<NewAugmentation> {
        Node<NewAugmentation>(
            id: id,
            outgoing: outgoing,
            incoming: incoming,
            augmentation: newAugmentation
        )
    }

    func mapAugmentation<NewAugmentation>(_ transform: (Augmentation) throws -> NewAugmentation) rethrows -> Node<NewAugmentation> {
        Node<NewAugmentation>(
            id: id,
            outgoing: outgoing,
            incoming: incoming,
            augmentation: try transform(augmentation)
        )
    }

    func withoutAugmentation() -> Node<NoAugmentation> {
        self.withAugmentation(NoAugmentation())
    }
    
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
