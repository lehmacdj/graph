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
    let value: Augmentation
}
