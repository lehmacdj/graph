//
//  GraphManager.swift
//  view
//
//  Created by Devin Lehmacher on 3/22/23.
//

import Foundation

/// Represents the  represents the base directory for a graph.
/// Offers a flyweight mechanism for retrieving Node model objects that represent nodes in the graph.
actor GraphManager: ObservableObject {
    let basePath: URL

    var nextNodeId: NID

    private var cloudDocumentsPull: Task<Void, Never>?

    init?(dir: URL) async {
        basePath = dir
        nextNodeId = 1
        if !FileManager.default.fileExists(atPath: metaPath(for: NID.origin).path) {
            logWarn("couldn't find origin node meta data file")
            return nil
        }
        let dataPath = dataPath(for: NID.origin)
        if !FileManager.default.fileExists(atPath: dataPath.path),
           let data = try? Data(contentsOf: dataPath),
           let string = String(data: data, encoding: .utf8),
           let nextNodeId = Int(string)
        {
            self.nextNodeId = nextNodeId
        } else {
            nextNodeId = NID.origin
            cloudDocumentsPull = await pullCloudDocuments()
        }
    }

    func updateMaxNodeId(from url: URL) {
        let lastPathComponent = url.lastPathComponent
        if lastPathComponent.hasSuffix(".json"),
           let dotIx = lastPathComponent.lastIndex(of: ".") {
            let dotPrevIx = lastPathComponent.index(before: dotIx)
            if let id = Int(url.lastPathComponent[...dotPrevIx]),
               id >= nextNodeId {
                nextNodeId = id + 1
            }
        }
    }

    func refresh() async {
        if let cloudDocumentsPull {
            cloudDocumentsPull.cancel()
            self.cloudDocumentsPull = await pullCloudDocuments()
        }
    }

    @MainActor
    func pullCloudDocuments() async -> Task<Void, Never> {
        let query = NSMetadataQuery()
        query.searchScopes = [NSMetadataQueryAccessibleUbiquitousExternalDocumentsScope, basePath]
        query.predicate = NSPredicate(value: true)
        @Sendable func updateWithQueryResults() async {
            query.disableUpdates()
            for result in query.results as! [NSMetadataItem] {
                let url = result.value(forAttribute: NSMetadataItemURLKey) as! URL
                await updateMaxNodeId(from: url)
            }
            query.enableUpdates()
        }
        let semaphor = Semaphor(initialCount: 0)
        let task: Task<Void, Never> = Task {
            for await _ in NotificationCenter.default.notifications(named: .NSMetadataQueryDidFinishGathering) {
                logInfo("received initial query results")
                await updateWithQueryResults()
                await semaphor.signal()
                break
            }
            for await _ in NotificationCenter.default.notifications(named: .NSMetadataQueryDidUpdate) {
                logInfo("received query result update")
                await updateWithQueryResults()
            }
            logInfo("completed (almost certainly because Task was cancelled)")
        }
        query.start()
        logInfo("started query to fetch iCloud Document contents")
        await semaphor.wait()
        return task
    }

    nonisolated func metaPath(for nid: NID) -> URL {
        basePath.appendingPathComponent(nid.metaPath)
    }

    nonisolated func dataPath(for nid: NID) -> URL {
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
            logError("origin node doesn't exist")
            fatalError("origin node doesn't exist")
        }
        return origin
    }

    var tags: Tags? {
        guard let tags = self[NID.tags],
              let tagIncoming = tags.meta.incoming["tags"],
              tagIncoming.contains(NID.origin) else {
            logWarn("no tags node found")
            return nil
        }
        return Tags(tagNode: tags, root: self)
    }

    /// Creates a new node not connected to anything
    func createNewNode() -> Node {
        let newNodeId = nextNodeId
        let newMeta = NodeMeta(id: newNodeId, incoming: [:], outgoing: [:])
        guard let data: Data = try? JSONEncoder().encode(newMeta) else {
            logError("failed to encode JSON for NodeMeta")
            fatalError("couldn't create a node")
        }
        guard FileManager.default.createFile(atPath: metaPath(for: newNodeId).path, contents: data) else {
            logError("failed to create file for new node")
            fatalError("failed to create a file")
        }
        guard let node = self[newNodeId] else {
            logError("couldn't access newly created node")
            fatalError("couldn't create a node")
        }
        nextNodeId = nextNodeId + 1
        do {
            try Data(String(nextNodeId).utf8).write(to: dataPath(for: NID.origin))
        } catch {
            logError("failed to update nextNodeId")
            fatalError("failed to update nextNodeId")
        }
        return node
    }

    func addLink(from start: NID, to end: NID, via transition: String) {
        guard let start = self[start],
              let end = self[end] else {
            return
        }

        addLink(from: start, to: end, via: transition)
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

    func removeLink(from start: NID, to end: NID, via transition: String) {
        guard let start = self[start],
              let end = self[end] else {
            return
        }

        removeLink(from: start, to: end, via: transition)
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
                logWarn("parent (\(parent.nid)) of \(node.nid) is missing")
            }
        }

        for child in node.outgoing {
            if let childNode = self[child.nid] {
                var childMeta = childNode.meta
                childMeta.incoming[child.transition] = childMeta.outgoing[child.transition]?.removing(node.nid)
                childNode.meta = childMeta
            } else {
                logWarn("child (\(child.nid)) of \(node.nid) is missing")
            }
        }

        node.deleteMetaAndData()
    }
}
