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

/// Represents the  represents the base directory for a graph.
/// Offers a flyweight mechanism for retrieving Node model objects that represent nodes in the graph.
class Graph {
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

    // need to use NSMapTable here because it supports weak references which is important for
    // this cache so that we don't end up retaining nodes longer than we need to
    private var internedNodes = NSMapTable<NSNumber, Node>(keyOptions: .copyIn, valueOptions: .weakMemory)

    subscript(id: NID) -> Node? {
        get {
            if let node = internedNodes.object(forKey: NSNumber(value: id)) {
                return node
            } else if let node = Node(nid: id, root: self) {
                internedNodes.setObject(node, forKey: NSNumber(value: id))
                return node
            } else {
                return nil
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

    var tagOptions: Set<String> {
        return Set(tagNode.outgoing)
    }

    func modify(for node: Node, adding toAdd: Set<String>, removing toRemove: Set<String>) {
        debug("node: \(node.nid), adding: \(toAdd), removing: \(toRemove)")
        assert(toAdd.intersection(toRemove).isEmpty, "can't add and remove the same tag")

        var tagMeta = tagNode.meta
        var nodeMeta = node.meta

        func setLikeAppend(_ mnids: [NID]?, elem nid: NID) -> [NID] {
            var nids = Set(mnids ?? [])
            nids.insert(nid)
            return [NID](nids)
        }

        for tag in toAdd {
            tagMeta.outgoing[tag] = setLikeAppend(tagMeta.outgoing[tag], elem: node.nid)
            nodeMeta.incoming[tag] = setLikeAppend(nodeMeta.incoming[tag], elem: tagNode.nid)
        }

        func setLikeRemove(_ mnids: [NID]?, elem nid: NID) -> [NID] {
            var nids = Set(mnids ?? [])
            nids.remove(nid)
            return [NID](nids)
        }

        for tag in toRemove {
            tagMeta.outgoing[tag] = setLikeRemove(tagMeta.outgoing[tag], elem: node.nid)
            nodeMeta.incoming[tag] = setLikeRemove(nodeMeta.incoming[tag], elem: tagNode.nid)
        }

        // we need to specially account for the case where node = tagNode because the
        // aliasing would cause us to loose half of the updates
        if node.nid == tagNode.nid {
            // all we need to do to account for the aliasing is to set the incoming correctly
            // this works because we only modify incoming for nodeMeta and not outgoing
            // (and we only modify outgoing for tagNode and not incoming)
            tagMeta.incoming = nodeMeta.incoming
        } else {
            node.meta = nodeMeta
        }

        tagNode.meta = tagMeta
    }
}

/// Eventually this will replace Node, and nodes will always monitor the filesystem.
class Node: ObservableObject {
    // MARK: node metadata

    let nid: NID

    var meta: NodeMeta {
        get { _meta }
        set {
            guard let data: Data = try? JSONEncoder().encode(newValue) else {
                warn("failed to encode JSON for NodeMeta")
                return
            }

            do {
                // suspend notifications when writing so we don't get a spurious one
                metaChangeSource.suspend()
                try data.write(to: root.metaPath(for: nid), options: .atomic)
                metaChangeSource.resume()

                // by not changing _meta until after having written the file successfully
                // we ensure that the view represents the actual state of the world
                _meta = newValue
            } catch {
                warn("failed writing data for node to disk")
            }
        }
    }

    var outgoing: [String] { Array(meta.outgoing.keys) }

    subscript(transition: String) -> [Node] {
        guard let ids = meta.outgoing[transition] else {
            warn("didn't find transition \(transition) from node \(meta.id)")
            return []
        }
        return ids.compactMap { root[$0] }
    }

    var tags: Set<String> {
        get {
            return Set(
                meta.incoming
                    .filter { $0.value.contains(NID.tags) }
                    .keys)
        }
        set {
            guard let tags = root.tags else {
                warn("trying to set tags, but there is no tags node")
                return
            }

            let oldTags = self.tags
            let newTags = newValue

            tags.modify(
                for: self,
                adding: newTags.subtracting(oldTags),
                removing: oldTags.subtracting(newTags))
        }
    }

    // MARK: data

    var dataUrl: URL? {
        let dataUrl = root.dataPath(for: nid)
        return FileManager().fileExists(atPath: dataUrl.path) ? dataUrl : nil
    }
    var data: Data? { dataUrl.flatMap({ try? Data(contentsOf: $0)}) }

    // MARK: private

    // this is the backing node metadata for this node. We keep it private to
    // encourage users of the node api to make all of the changes to a node
    // they want to at once and then set it. This is more performant, since
    // every write to meta results in a file write
    // this initialized last by method call and will never be nil after successful
    // initialization because constructor would return nil if this failed to initialize
    @Published private var _meta: NodeMeta!

    // TODO: make this private / push through the consequences of that
    let root: Graph

    private let metaChangeSource: DispatchSourceFileSystemObject
    private let metaHandle: FileHandle

    /// Prefer initializing this via a factory method that constructs it from a static Node or from the Root directly
    fileprivate init?(nid: NID, root: Graph) {
        self.nid = nid
        self.root = root

        let metaFileDescriptor = open(root.metaPath(for: nid).path, O_EVTONLY | O_RDONLY)
        if metaFileDescriptor == -1 {
            warn("metadata file for node \(nid) does not exist or cannot be opened")
            return nil
        }
        self.metaHandle = FileHandle(fileDescriptor: metaFileDescriptor)
        self.metaChangeSource =
            DispatchSource.makeFileSystemObjectSource(
                fileDescriptor: self.metaHandle.fileDescriptor,
                eventMask: .write,
                queue: DispatchQueue.main)

        guard let initialMeta = tryGetMeta() else {
            return nil
        }

        self._meta = initialMeta

        metaChangeSource.setEventHandler { [weak self] in
            if let meta = self?.tryGetMeta() {
                self?._meta = meta
            }
        }

        metaChangeSource.activate()
    }

    deinit {
        do {
            try metaHandle.close()
        } catch {
            warn("unexpected error while closing a file")
        }
    }

    private func tryGetMeta() -> NodeMeta? {
        do {
            try metaHandle.seek(toOffset: 0)
        } catch {
            warn("failed to seek file")
            return nil
        }

        guard let metaContents = try? metaHandle.readToEnd() else {
            warn("couldn't access file contents for nid: \(nid)")
            return nil
        }
        guard let meta = try? JSONDecoder().decode(NodeMeta.self, from: metaContents) else {
            warn("couldn't decode json data")
            return nil
        }

        return meta
    }
}

extension NID {
    var metaPath: String { return "\(self).json" }
    var dataPath: String { return "\(self).data" }
}

struct NodeMeta {
    var id: NID
    var incoming: [String:[NID]]
    var outgoing: [String:[NID]]
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
