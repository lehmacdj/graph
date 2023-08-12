//
//  DocumentNode.swift
//  view
//
//  Created by Devin Lehmacher on 6/18/23.
//

import Combine
import UIKit
import UniformTypeIdentifiers

final class DocumentNode: UIDocument, GraphManagerNode {

    // MARK: Node

    let nid: NID
    let manager: GraphManager<DocumentNode>

    struct FailedToOpenDocument: LocalizedError, Codable {
        let documentClosed: Bool
        let inConflict: Bool
        let savingError: Bool
        let editingDisabled: Bool
        let progressAvailable: Bool

        init(documentState: UIDocument.State) {
            documentClosed = documentState.contains(.closed)
            inConflict = documentState.contains(.inConflict)
            savingError = documentState.contains(.savingError)
            editingDisabled = documentState.contains(.editingDisabled)
            progressAvailable = documentState.contains(.progressAvailable)
        }
    }

    init(nid: NID, root: GraphManager<DocumentNode>) async throws {
        self.nid = nid
        self.manager = root
        super.init(fileURL: manager.metaPath(for: nid))
        guard await super.open() else {
            throw FailedToOpenDocument(documentState: documentState)
        }
    }

    /// Initialized with initially invalid information, but we read before returning from the constructor so it's never invalid to an outside observer
    @Published var meta: NodeMeta = .init(id: -1, incoming: [:], outgoing: [:])

    var metaPublisher: AnyPublisher<NodeMeta, Never> {
        $meta.eraseToAnyPublisher()
    }

    subscript(transition: String) -> [DocumentNode] {
        get async {
            guard let ids = meta.outgoing[transition] else {
                logDebug("didn't find transition \(transition) from node \(meta.id)")
                return []
            }
            return await Array(ids.async.compactMap { [weak self] in try? await self?.manager[$0] })
        }
    }

    var dataURL: URL? {
        guard nid != NID.origin else {
            // even though origin has some info about maxNodeId we want to pretend that doesn't
            // exist. We want to eventually get rid of that data and allow the origin to potentially
            // have it's own data again in the future
            return nil
        }

        let dataURL = manager.dataPath(for: nid)
        guard !FileManager().fileExists(atPath: dataURL.absoluteString) else {
            return dataURL
        }

        do {
            let attributes = try dataURL.resourceValues(forKeys: [.isUbiquitousItemKey])
            if let isUbiquitousItem = attributes.isUbiquitousItem, isUbiquitousItem {
                return dataURL
            } else {
                // attributes.isUbiquitousItem is nil when the file does not exist
                return nil
            }
        } catch {
            logWarn("failed to determine if item exists in iCloud: \(error)")
            return nil
        }
    }

    var dataRequiresDownload: Bool {
        do {
            let dataURL = try dataURL.unwrapped("need data URL to know if data exists")
            let attributes = try dataURL.resourceValues(forKeys: [.ubiquitousItemDownloadingStatusKey])
            let downloadingStatus = try attributes.ubiquitousItemDownloadingStatus.unwrapped("we requested .ubiquitousItemDownloadingStatusKey")
            return downloadingStatus != .current
        } catch {
            logWarn("failed to determine if file needs to be downloaded: \(error)")
            return false
        }
    }

    var data: DataDocument<DocumentNode>? {
        get async {
            await DataDocument(node: self)
        }
    }

    func deleteMetaAndData() {
        let fileManager = FileManager()
        do {
            try fileManager.removeItem(at: manager.metaPath(for: nid))
            if let dataURL {
                try fileManager.removeItem(at: dataURL)
            }
        } catch {
            logWarn("failed while removing a file")
        }
    }

    // MARK: UIDocument

    struct InvalidFileType: LocalizedError, Codable {}

    override func contents(forType typeName: String) throws -> Any {
        guard typeName == UTType.json.identifier else {
            throw InvalidFileType()
        }
        let encoder = JSONEncoder()
        return try encoder.encode(meta)
    }

    struct ContentsIsNotData: LocalizedError, Codable {}

    override func load(fromContents contents: Any, ofType typeName: String?) throws {
        guard typeName == UTType.json.identifier else {
            throw InvalidFileType()
        }
        guard let data = contents as? Data else {
            throw ContentsIsNotData()
        }
        meta = try JSONDecoder().decode(NodeMeta.self, from: data)
    }
}
