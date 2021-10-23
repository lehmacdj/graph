//
//  GraphNode.swift
//  graph-view
//
//  Created by Devin Lehmacher on 10/10/21.
//

import Foundation

typealias NID = Int

struct Root {
    let dir: URL
    let basePath: URL
    
    init(dir: URL) {
        self.dir = dir
        basePath = dir
    }
    
    func metaPath(for nid: NID) -> URL {
        basePath.appendingPathComponent(nid.metaPath)
    }
    
    func dataPath(for nid: NID) -> URL {
        basePath.appendingPathComponent(nid.dataPath)
    }
    
    subscript(id: NID) -> Node? {
        guard let metaContents = try? Data(contentsOf: metaPath(for: id)) else {
            warn("couldn't access file contents for nid: \(id)")
            return nil
        }
        guard let meta = try? JSONDecoder().decode(NodeMeta.self, from: metaContents) else {
            warn("couldn't decode json data")
            return nil
        }
        let dataUrl = dataPath(for: id)
        let mDataUrl = FileManager().fileExists(atPath: dataUrl.path) ? dataUrl : nil
        return Node(root: self, meta: meta, dataUrl: mDataUrl)
    }
    
    var origin: Node {
        guard let origin = self[0] else {
            error("origin node doesn't exist")
            fatalError("origin node doesn't exist")
        }
        return origin
    }
    
    var tags: Node? {
        guard let tags = self[1], tags.meta.incoming["tags"] == 0 else {
            warn("no tags node found")
            return nil
        }
        return tags
    }
}

struct Node {
    fileprivate let root: Root
    let meta: NodeMeta
    let dataUrl: URL?
    var data: Data? { dataUrl.flatMap({ try? Data(contentsOf: $0)}) }
    var outgoing: [String] {
        return Array(meta.outgoing.keys)
    }
    subscript(transition: String) -> Node? {
        guard let id = meta.outgoing[transition] else {
            warn("didn't find node for transition \(transition) from node \(meta.id)")
            return nil
        }
        return root[id]
    }
}

extension NID {
    var metaPath: String { return "\(self).json" }
    var dataPath: String { return "\(self).data" }
}

struct NodeMeta {
    let id: NID
    let incoming: [String:NID]
    let outgoing: [String:NID]
}

struct ConnectDTO: Decodable {
    let transition: String
    let id: NID
    enum CodingKeys: String, CodingKey {
        case transition = "_connectTransition"
        case id = "_connectNode"
    }
}

struct NodeDTO: Decodable {
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

extension NodeMeta: Decodable {
    enum NodeKeys: String, CodingKey {
        case id
        case incoming
        case outgoing
    }
    
    public init(from decoder: Decoder) throws {
        let dto = try NodeDTO(from: decoder)
        id = dto.id
        var incomingDict = [String:NID](minimumCapacity: dto.incoming.count)
        for c in dto.incoming {
            incomingDict[c.transition] = c.id
        }
        incoming = incomingDict
        var outgoingDict = [String:NID](minimumCapacity: dto.outgoing.count)
        for c in dto.outgoing {
            outgoingDict[c.transition] = c.id
        }
        outgoing = outgoingDict
    }
}
