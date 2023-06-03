//
//  GraphNode.swift
//  graph-view
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

struct Tags {
    let tagNode: Node
    let root: GraphManager

    var tagOptions: Set<String> {
        return Set(tagNode.meta.outgoing.keys)
    }

    var tagNids: Set<NID> {
        return Set(tagNode.meta.outgoing.values.flatMap { $0 })
    }

    func modify(for node: Node, adding toAdd: Set<String>, removing toRemove: Set<String>) async {
        logDebug("node: \(node.nid), adding: \(toAdd), removing: \(toRemove)")
        assert(toAdd.intersection(toRemove).isEmpty, "can't add and remove the same tag")

        for tag in toAdd {
            let tagContainers = await tagNode[tag]
            if tagContainers.isEmpty {
                logWarn("can't create new node yet")
                continue
            } else if tagContainers.count > 1 {
                logWarn("more than one node with same transition as tag, skipping: \(tag)")
                continue
            }
            let tagContainer = tagContainers[0]
            await root.addLink(from: tagContainer, to: node, via: "")
        }

        for tag in toRemove {
            let tagContainers = await tagNode[tag]
            if tagContainers.isEmpty {
                logWarn("can't create new node yet")
                continue
            } else if tagContainers.count > 1 {
                logWarn("more than one node with same transition as tag, skipping: \(tag)")
                continue
            }
            let tagContainer = tagContainers[0]
            await root.removeLink(from: tagContainer, to: node, via: "")
        }
    }
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

private extension DecodingError {
    /// Helper that returns a generic decoding error given a codingPath and a detailed description.
    /// Do try to use one of the more specific overloads first if you want to use this though.
    static func dataCorruptedError(for codingPath: [CodingKey], debugDescription: String) -> DecodingError {
        return DecodingError.dataCorrupted(Context(codingPath: codingPath, debugDescription: debugDescription))
    }
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
