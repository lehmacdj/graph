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

    func addLink(from startNode: Node, to endNode: Node, via transition: String) {
        // specially handle the case of a single node because otherwise
        // we would overwrite only half of the metadata
        if startNode.nid == endNode.nid {
            // there is only actually one node
            let node = startNode
            var nodeMeta = startNode.meta
            nodeMeta.outgoing[transition] = (nodeMeta.outgoing[transition] ?? Set()).inserting(node.nid)
            nodeMeta.incoming[transition] = (nodeMeta.incoming[transition] ?? Set()).inserting(node.nid)
            node.meta = nodeMeta
        } else {
            var startMeta = startNode.meta
            var endMeta = endNode.meta
            startMeta.outgoing[transition] = (startMeta.outgoing[transition] ?? Set()).inserting(endNode.nid)
            endMeta.incoming[transition] = (endMeta.incoming[transition] ?? Set()).inserting(startNode.nid)
            startNode.meta = startMeta
            endNode.meta = endMeta
        }
    }

    func removeLink(from startNode: Node, to endNode: Node, via transition: String) {
        // specially handle the case of a single node because otherwise
        // we would overwrite only half of the metadata
        if startNode.nid == endNode.nid {
            // there is only actually one node
            let node = startNode
            var nodeMeta = startNode.meta
            nodeMeta.outgoing[transition] = nodeMeta.outgoing[transition]?.removing(node.nid)
            nodeMeta.incoming[transition] = nodeMeta.incoming[transition]?.removing(node.nid)
            node.meta = nodeMeta
        } else {
            var startMeta = startNode.meta
            var endMeta = endNode.meta
            startMeta.outgoing[transition] = startMeta.outgoing[transition]?.removing(endNode.nid)
            endMeta.incoming[transition] = endMeta.incoming[transition]?.removing(startNode.nid)
            startNode.meta = startMeta
            endNode.meta = endMeta
        }
    }

    func forceRemove(node: Node) {
        for parent in node.incoming {
            if let parentNode = self[parent.nid] {
                var parentMeta = parentNode.meta
                parentMeta.outgoing[parent.transition] = parentMeta.outgoing[parent.transition]?.removing(node.nid)
                parentNode.meta = parentMeta
            } else {
                warn("parent (\(parent.nid)) of \(node.nid) is missing")
            }
        }

        for child in node.outgoing {
            if let childNode = self[child.nid] {
                var childMeta = childNode.meta
                childMeta.incoming[child.transition] = childMeta.outgoing[child.transition]?.removing(node.nid)
                childNode.meta = childMeta
            } else {
                warn("child (\(child.nid)) of \(node.nid) is missing")
            }
        }

        node.deleteMetaAndData()
    }
}

struct Tags {
    let tagNode: Node

    var tagOptions: Set<String> {
        return Set(tagNode.meta.outgoing.keys)
    }

    var tagNids: Set<NID> {
        return Set(tagNode.meta.outgoing.values.flatMap { $0 })
    }

    func modify(for node: Node, adding toAdd: Set<String>, removing toRemove: Set<String>) {
        debug("node: \(node.nid), adding: \(toAdd), removing: \(toRemove)")
        assert(toAdd.intersection(toRemove).isEmpty, "can't add and remove the same tag")

        for tag in toAdd {
            let tagContainers = tagNode[tag]
            if tagContainers.isEmpty {
                warn("can't create new node yet")
                continue
            } else if tagContainers.count > 1 {
                warn("more than one node with same transition as tag: \(tag)")
                continue
            }
            let tagContainer = tagContainers[0]
            tagNode.root.addLink(from: tagContainer, to: node, via: "")
        }

        for tag in toRemove {
            let tagContainers = tagNode[tag]
            if tagContainers.isEmpty {
                warn("can't create new node yet")
                continue
            } else if tagContainers.count > 1 {
                warn("more than one node with same transition as tag: \(tag)")
                continue
            }
            let tagContainer = tagContainers[0]
            tagNode.root.removeLink(from: tagContainer, to: node, via: "")
        }
    }
}

struct NodeTransition: Identifiable, Comparable {
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

    var outgoing: [NodeTransition] {
        var result = [NodeTransition]()
        for (transition, nids) in meta.outgoing {
            for nid in nids {
                result.append(NodeTransition(transition: transition, nid: nid))
            }
        }
        return result.sorted()
    }

    var incoming: [NodeTransition] {
        var result = [NodeTransition]()
        for (transition, nids) in meta.incoming {
            for nid in nids {
                result.append(NodeTransition(transition: transition, nid: nid))
            }
        }
        return result.sorted()
    }

    subscript(transition: String) -> [Node] {
        guard let ids = meta.outgoing[transition] else {
            warn("didn't find transition \(transition) from node \(meta.id)")
            return []
        }
        return ids.compactMap { root[$0] }
    }

    var tags: Set<String> {
        get {
            let tagContainers = Set(
                meta.incoming
                    .filter { !$0.value.intersection(root.tags?.tagNids ?? Set()).isEmpty }
                    .values
                    .flatMap { $0 })
            var result = Set<String>()
            for tagContainer in tagContainers {
                if let tagContainer = root[tagContainer] {
                    for tag in tagContainer.meta.incoming.filter({ $0.value.contains(NID.tags) }).keys {
                        result.insert(tag)
                    }
                }
            }
            return result
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

    // MARK: fileprivate

    fileprivate func deleteMetaAndData() {
        let fileManager = FileManager()
        do {
            try fileManager.removeItem(at: root.metaPath(for: nid))
            if let dataUrl = dataUrl {
                try fileManager.removeItem(at: dataUrl)
            }
        } catch {
            warn("failed while removing a file")
        }
    }

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
