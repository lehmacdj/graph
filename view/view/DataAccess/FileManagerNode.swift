//
//  Node.swift
//  view
//
//  Created by Devin Lehmacher on 3/26/23.
//

import Foundation

final class FileManagerNode: ObservableObject, GraphManagerNode {
    // MARK: node metadata

    let nid: NID

    var meta: NodeMeta {
        get { _meta }
        set {
            guard let data: Data = try? JSONEncoder().encode(newValue) else {
                logWarn("failed to encode JSON for NodeMeta")
                return
            }

            do {
                // suspend notifications when writing so we don't get a spurious one
                metaChangeSource.suspend()
                try data.write(to: manager.metaPath(for: nid), options: .atomic)
                metaChangeSource.resume()

                // by not changing _meta until after having written the file successfully
                // we ensure that the view represents the actual state of the world
                _meta = newValue
            } catch {
                logWarn("failed writing data for node to disk")
            }
        }
    }


    // MARK: data

    var dataURL: URL? {
        guard nid != NID.origin else {
            // even though origin has some info about maxNodeId we want to pretend that doesn't
            // exist. We want to eventually get rid of that data and allow the origin to potentially
            // have it's own data again in the future
            return nil
        }
        let dataURL = manager.dataPath(for: nid)
        return FileManager.default.fileExists(atPath: dataURL.path) ? dataURL : nil
    }

    var dataRequiresDownload: Bool {
        // defaulting to false preserves the old behavior
        // if everything goes well, should be deleting FileManagerNode soon anyways
        false
    }

    var data: DataDocument<FileManagerNode>? {
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

    // MARK: private

    // this is the backing node metadata for this node. We keep it private to
    // encourage users of the node api to make all of the changes to a node
    // they want to at once and then set it. This is more performant, since
    // every write to meta results in a file write
    // this initialized last by method call and will never be nil after successful
    // initialization because constructor would return nil if this failed to initialize
    @Published private var _meta: NodeMeta!

    // TODO: make this private / push through the consequences of that
    let manager: GraphManager<FileManagerNode>

    private let metaChangeSource: DispatchSourceFileSystemObject
    private let metaHandle: FileHandle

    /// Prefer initializing this via a factory method that constructs it from a static Node or from the Root directly
    init?(nid: NID, root: GraphManager<FileManagerNode>) async {
        self.nid = nid
        self.manager = root

        let metaFileDescriptor = open(root.metaPath(for: nid).path, O_EVTONLY | O_RDONLY)
        if metaFileDescriptor == -1 {
            logWarn("metadata file for node \(nid) does not exist or cannot be opened")
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
            logWarn("unexpected error while closing a file")
        }
    }

    private func tryGetMeta() -> NodeMeta? {
        do {
            try metaHandle.seek(toOffset: 0)
        } catch {
            logWarn("failed to seek file")
            return nil
        }

        guard let metaContents = try? metaHandle.readToEnd() else {
            logWarn("couldn't access file contents for nid: \(nid)")
            return nil
        }
        guard let meta = try? JSONDecoder().decode(NodeMeta.self, from: metaContents) else {
            logWarn("couldn't decode json data")
            return nil
        }

        return meta
    }
}
