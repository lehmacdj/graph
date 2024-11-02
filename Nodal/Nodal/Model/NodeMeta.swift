//
//  NodeMeta.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/24.
//

struct NodeMeta {
    var id: NID
    var incoming: [String:Set<NID>]
    var outgoing: [String:Set<NID>]
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
