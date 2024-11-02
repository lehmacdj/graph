//
//  NodeDTO.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/24.
//

struct NodeDTO: Decodable, Encodable {
    let id: NID
    let incoming: [ConnectDTO]
    let outgoing: [ConnectDTO]
}
