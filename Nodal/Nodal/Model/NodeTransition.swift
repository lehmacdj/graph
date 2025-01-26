//
//  NodeTransition.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/24.
//

struct NodeTransition: Identifiable, Hashable, Sendable {
    let transition: String
    let nid: NID

    var id: some Hashable {
        return "\(transition)\(nid)"
    }
}
