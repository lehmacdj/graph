//
//  GraphNode.swift
//  Nodal
//
//  Created by Devin Lehmacher on 10/10/21.
//

import Combine
import Foundation

public let nidDigits = 12

extension Character {
    var isBase62Digit: Bool {
        isASCII && (isLetter || isNumber)
    }
}

struct NID: Equatable, Hashable {
    let representation: String

    init?(representation: String) {
        guard representation.count == nidDigits && representation.allSatisfy({$0.isBase62Digit}) else {
            return nil
        }
        self.representation = representation
    }
}

extension NID: Encodable, Decodable {
    public init(from decoder: Decoder) throws {
        let representation = try String(from: decoder)
        if let nid = NID(representation: representation) {
            self = nid
        } else {
            throw DecodingError.dataCorrupted(
                .init(
                    codingPath: decoder.codingPath,
                    debugDescription: "Invalid NID representation: \(representation)"
                )
            )
        }
    }

    public func encode(to encoder: Encoder) throws {
        try representation.encode(to: encoder)
    }
}

extension NID {
    private static let base62Characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

    static func random() -> NID {
        let randomString = String((0..<nidDigits).map { _ in base62Characters.randomElement()! })
        return NID(representation: randomString)!
    }
}

enum SpecialNode: String, CaseIterable {
    case origin
    case systemNode = "system-node"
    case tags
    case importUrls = "import-urls"
    case importDates = "import-dates"
    case fileHashes = "file-hashes"

    var nid: NID {
        // we statically know that all of these match the representation, thus they won't crash
        switch self {
        case .origin: NID(representation: "000000000000")!
        case .systemNode: NID(representation: "0daCJjMrQel8")!
        case .tags: NID(representation: "pbYxBO6fzBQV")!
        case .importUrls: NID(representation: "a0fVkm0kR7KE")!
        case .importDates: NID(representation: "S00KkOYoVpFu")!
        case .fileHashes: NID(representation: "AhQufiPzgyRf")!
        }
    }
}

extension NID {
    static let origin: NID = SpecialNode.origin.nid
    static let systemNode: NID = SpecialNode.systemNode.nid
    static let tags: NID = SpecialNode.tags.nid
    static let importUrls: NID = SpecialNode.importUrls.nid
    static let importDates: NID = SpecialNode.importDates.nid
    static let fileHashes: NID = SpecialNode.fileHashes.nid
}

extension NID: CustomStringConvertible {
    var description: String {
        return "#\(representation)"
    }
}

struct NodeTransition: Identifiable, Hashable {
    let transition: String
    let nid: NID

    var id: some Hashable {
        return "\(transition)\(nid)"
    }
}

/// Eventually this will replace Node, and nodes will always monitor the filesystem.

extension NID {
    var metaPath: String { return "\(representation).json" }
    var dataPath: String { return "\(representation).data" }
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
