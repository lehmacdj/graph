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

/// Root represents the base directory for a graph. It also offers some conveniences for accessing some standardized parts of the graph.
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
            return Node(nid: id, root: self)
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

/// Eventually this will replace Node, and nodes will always monitor the filesystem.
class Node {
    // MARK: node metadata

    let nid: NID

    // this initialized last by method call and will never be nil after successful
    // initialization because constructor would return nil if this failed to initialize
    @Published var meta: NodeMeta!

    var outgoing: [String] { Array(meta.outgoing.keys) }

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

    // MARK: data

    var dataUrl: URL? {
        let dataUrl = root.dataPath(for: nid)
        return FileManager().fileExists(atPath: dataUrl.path) ? dataUrl : nil
    }
    var data: Data? { dataUrl.flatMap({ try? Data(contentsOf: $0)}) }

    // MARK: private

    // TODO: make this private / push through the consequences of that
    let root: Root

    private let metaChangeSource: DispatchSourceFileSystemObject
    private let metaHandle: FileHandle

    /// Prefer initializing this via a factory method that constructs it from a static Node or from the Root directly
    fileprivate init?(nid: NID, root: Root) {
        self.nid = nid
        self.root = root

        let metaFileDescriptor = open(root.metaPath(for: nid).path, O_EVTONLY | O_RDONLY)
        self.metaHandle = FileHandle(fileDescriptor: metaFileDescriptor)
        self.metaChangeSource =
            DispatchSource.makeFileSystemObjectSource(
                fileDescriptor: self.metaHandle.fileDescriptor,
                eventMask: .write,
                queue: DispatchQueue.main)

        guard let initialMeta = tryGetMeta() else {
            return nil
        }

        self.meta = initialMeta

        metaChangeSource.setEventHandler { [weak self] in
            if let meta = self?.tryGetMeta() {
                self?.meta = meta
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
