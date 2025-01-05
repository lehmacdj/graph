//
//  DocumentNode.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/18/23.
//

import Combine
import UIKit

final class DocumentNode: GraphManagerNode {
    let nid: NID
    let manager: GraphManager<DocumentNode>

    init(nid: NID, root: GraphManager<DocumentNode>) async throws {
        self.nid = nid
        self.manager = root
        self._metadataDocument = try await NodeMetadataDocument(metaURL: manager.metaPath(for: nid))
    }

    deinit {
        Task { [_metadataDocument, _dataDocument] in
            async let _ = _metadataDocument?.close()
            async let _ = _dataDocument?.close()
            // TODO: send notification that something went wrong when closing document through GraphManager
            // then we can surface the error to the user
        }
    }

    // MARK: Metadata

    private var _metadataDocument: NodeMetadataDocument?
    private var metadataDocument: NodeMetadataDocument {
        guard let _metadataDocument else {
            fatalError("initialized in constructor")
        }
        return _metadataDocument
    }


    var meta: NodeMeta {
        get {
            metadataDocument.meta
        }
        set {
            metadataDocument.meta = newValue
        }
    }

    var metaPublisher: AnyPublisher<NodeMeta, Never> {
        metadataDocument.metaPublisher
    }

    // MARK: Data

    var dataURL: URL? {
        guard nid != NID.origin else {
            // even though origin has some info about maxNodeId we want to pretend that doesn't
            // exist. We want to eventually get rid of that data and allow the origin to potentially
            // have it's own data again in the future
            return nil
        }

        let dataURL = manager.dataPath(for: nid)
        guard !FileManager().fileExists(atPath: dataURL.path) else {
            return dataURL
        }

        do {
            let attributes = try dataURL.resourceValues(forKeys: [.isUbiquitousItemKey])
            if let isUbiquitousItem = attributes.isUbiquitousItem, isUbiquitousItem {
                return dataURL
            } else {
                // attributes.isUbiquitousItem is typically only nil when the file does not exist
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
            return downloadingStatus != .current || downloadingStatus != .downloaded
        } catch {
            logWarn("failed to determine if file needs to be downloaded: \(error)")
            return false
        }
    }

    private var _dataDocument: DataDocument?
    private var dataDocument: DataDocument? {
        get async {
            if _dataDocument == nil {
                _dataDocument = try? await DataDocument(node: self)
            }
            return _dataDocument
        }
    }

    var data: Data? {
        get async {
            await dataDocument?.data
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
}
