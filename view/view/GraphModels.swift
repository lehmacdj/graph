//
//  GraphNode.swift
//  graph-view
//
//  Created by Devin Lehmacher on 10/10/21.
//

import Foundation

typealias NID = Int
extension NID {
    static let origin: NID = 0
    static let tags: NID = 1
}

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
        get {
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
        nonmutating set {
            // 
            guard let node = newValue else {
                error("must specify node when writing; but was nil for nid: \(id)")
                return
            }
            guard node.meta.id == id else {
                error("mismatch in id of node being written \(node.meta.id) and place being written to \(id)")
                return
            }
            guard let metaContents = try? JSONEncoder().encode(node.meta) else {
                error("failed to encode node: \(id)")
                return
            }
            guard let _ = try? metaContents.write(to: metaPath(for: node.meta.id)) else {
                error("failed to write node metadata for \(id)")
                return
            }
        }
    }

    var origin: Node {
        guard let origin = self[NID.origin] else {
            error("origin node doesn't exist")
            fatalError("origin node doesn't exist")
        }
        return origin
    }

    var tags: Tags? {
        guard let tags = self[NID.tags],
              let tagIncoming = tags.meta.incoming["tags"],
              tagIncoming.contains(NID.origin) else {
            warn("no tags node found")
            return nil
        }
        return Tags(tagNode: tags)
    }
}

struct Tags {
    let tagNode: Node
    
    var tagOptions: [String] {
        return tagNode.outgoing
    }
}

struct Node {
    let root: Root
    let meta: NodeMeta
    let dataUrl: URL?

    var data: Data? { dataUrl.flatMap({ try? Data(contentsOf: $0)}) }

    var outgoing: [String] {
        return Array(meta.outgoing.keys)
    }

    subscript(transition: String) -> [Node] {
        guard let ids = meta.outgoing[transition] else {
            warn("didn't find transition \(transition) from node \(meta.id)")
            return []
        }
        return ids.compactMap { root[$0] }
    }
    var tags: Set<String> {
        return Set(
            meta.incoming
                .filter { $0.value.contains(NID.tags) }
                .keys)
    }

    func withNewTags(_ newTags: Set<String>) -> Node {
        var meta = self.meta
        var newIncoming = meta.incoming.filter {
            !$1.contains(NID.tags)
        }
        for tag in newTags {
            newIncoming.appendSeq([NID.tags], toKey: tag)
        }
        meta.incoming = newIncoming
        return Node(root: root, meta: meta, dataUrl: dataUrl)
    }

    // Behavior of this function is broken because swift ui caches stuff too
    // long (rightfully, but I was hoping I could be cheeky and get away with it).
    // Fix below is probably necessary
    //
    // Extremely dirty function to perform side effects on FS representation
    // then return new node with modified stuff modified as change occurred
    // TODO: instead of this dirty hack, rewrite Node using Combine to
    // watch their file and update automatically when changed. Will need
    // to also watch for this change in the views somehow. Perhaps, this logic
    // should be in a Node wrapper type instead of node itself, although TBH
    // Node kind of is that for NodeMeta already.
    func settingTagsTo(_ newTags: Set<String>) -> Node {
        guard let tags = root.tags else {
            warn("mutating is impossible because there is no tags node")
            return self
        }
        var tagMeta = tags.tagNode.meta
        var tagNewOutgoing = tagMeta.outgoing.filter {
            !$1.contains(self.meta.id)
        }
        for tag in newTags {
            tagNewOutgoing.appendSeq([self.meta.id], toKey: tag)
        }
        tagMeta.outgoing = tagNewOutgoing
        root[NID.tags] = Node(root: root, meta: tagMeta, dataUrl: dataUrl)
        let newNode = self.withNewTags(newTags)
        root[self.meta.id] = newNode
        return newNode
    }
}

extension NID {
    var metaPath: String { return "\(self).json" }
    var dataPath: String { return "\(self).data" }
}

struct NodeMeta {
    var id: NID
    // TODO: this data structure is broken for cases when there
    // is more than one link from a given node
    // Maybe just represent as pairs with lookup function? Or could
    // do a [String:[NID]] possibly
    var incoming: [String:[NID]]
    var outgoing: [String:[NID]]
}

struct ConnectDTO: Decodable, Encodable {
    let transition: String
    let id: NID
    enum CodingKeys: String, CodingKey {
        case transition = "_connectTransition"
        case id = "_connectNode"
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
        incoming = incomingDict
        var outgoingDict = [String:[NID]](minimumCapacity: dto.outgoing.count)
        for c in dto.outgoing {
            outgoingDict.appendSeq([c.id], toKey: c.transition)
        }
        outgoing = outgoingDict
    }

    public func encode(to encoder: Encoder) throws {
        let incoming = self.incoming.flatMap { ts in ts.value.map { ConnectDTO(transition: ts.key, id: $0) } }
        let outgoing = self.outgoing.flatMap { ts in ts.value.map { ConnectDTO(transition: ts.key, id: $0) } }
        try NodeDTO(id: id, incoming: incoming, outgoing: outgoing).encode(to: encoder)
    }
}
