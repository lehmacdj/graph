//
//  SystemNodes.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/24.
//

enum SystemNodes: String, CaseIterable {
    case origin
    case systemNode = "system-node"
    case tags
    case importUrls = "import-urls"
    case importDates = "import-dates"
    case fileHashes = "file-hashes"
    case historicalSpecialNodes = "historical-special-nodes"
    case sequenceIDs = "sequence-ids"

    var nid: NID {
        // we statically know that all of these match the representation, thus they won't crash
        switch self {
        case .origin: NID(representation: "000000000000")!
        case .systemNode: NID(representation: "0daCJjMrQel8")!
        case .tags: NID(representation: "pbYxBO6fzBQV")!
        case .importUrls: NID(representation: "a0fVkm0kR7KE")!
        case .importDates: NID(representation: "S00KkOYoVpFu")!
        case .fileHashes: NID(representation: "AhQufiPzgyRf")!
        case .historicalSpecialNodes: NID(representation: "3JJvxFUHGAA1")!
        case .sequenceIDs: NID(representation: "VEfLhuTgZ88Z")!
        }
    }
}

extension NID {
    static let origin: NID = SystemNodes.origin.nid
    static let systemNode: NID = SystemNodes.systemNode.nid
    static let tags: NID = SystemNodes.tags.nid
    static let importUrls: NID = SystemNodes.importUrls.nid
    static let importDates: NID = SystemNodes.importDates.nid
    static let fileHashes: NID = SystemNodes.fileHashes.nid
}
