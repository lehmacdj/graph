//
//  GraphNode.swift
//  Nodal
//
//  Created by Devin Lehmacher on 10/10/21.
//

import Combine
import Foundation

typealias NID = Int
extension NID {
    static let origin: NID = 0
    static let tags: NID = 1
}

struct NodeTransition: Identifiable, Comparable, Hashable {
    let transition: String
    let nid: NID

    var id: some Hashable {
        return "\(transition)\(nid)"
    }

    static func < (lhs: NodeTransition, rhs: NodeTransition) -> Bool {
        return lhs.transition < rhs.transition
        || lhs.transition == rhs.transition && lhs.nid < rhs.nid
    }
}

/// Eventually this will replace Node, and nodes will always monitor the filesystem.

extension NID {
    var metaPath: String { return "\(self).json" }
    var dataPath: String { return "\(self).data" }
}

struct NodeMeta {
    var id: NID
    var incoming: [String:Set<NID>]
    var outgoing: [String:Set<NID>]
}

struct ConnectDTO: Decodable, Encodable {
    let transition: String
    let id: NID
    enum CodingKeys: String, CodingKey {
        case transition = "t"
        case id = "n"
    }
}

struct NodeDTO: Decodable, Encodable {
    let id: NID
    let incoming: [ConnectDTO]
    let outgoing: [ConnectDTO]
}

extension NodeMeta: Decodable, Encodable {
    enum NodeKeys: String, CodingKey {
        case id
        case incoming
        case outgoing
    }

    public init(from decoder: Decoder) throws {
        let dto = try NodeDTO(from: decoder)
        id = dto.id
        var incomingDict = [String:[NID]](minimumCapacity: dto.incoming.count)
        for c in dto.incoming {
            incomingDict.appendSeq([c.id], toKey: c.transition)
        }
        incoming = incomingDict.mapValues { Set($0) }
        var outgoingDict = [String:[NID]](minimumCapacity: dto.outgoing.count)
        for c in dto.outgoing {
            outgoingDict.appendSeq([c.id], toKey: c.transition)
        }
        outgoing = outgoingDict.mapValues { Set($0) }
    }

    public func encode(to encoder: Encoder) throws {
        let incoming = self.incoming.flatMap { ts in ts.value.map { ConnectDTO(transition: ts.key, id: $0) } }
        let outgoing = self.outgoing.flatMap { ts in ts.value.map { ConnectDTO(transition: ts.key, id: $0) } }
        try NodeDTO(id: id, incoming: incoming, outgoing: outgoing).encode(to: encoder)
    }
}
