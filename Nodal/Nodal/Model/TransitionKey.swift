//
//  TransitionKey.swift
//  Nodal
//
//  Created by Devin Lehmacher on 12/15/24.
//

struct TransitionKey: Hashable {
    let transition: NodeTransition
    let direction: Direction

    init(_ transition: NodeTransition, direction: Direction) {
        self.transition = transition
        self.direction = direction
    }

    init(transition: String, destination: NID, direction: Direction) {
        self.transition = NodeTransition(transition: transition, nid: destination)
        self.direction = direction
    }
}

