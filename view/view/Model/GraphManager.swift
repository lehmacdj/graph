//
//  GraphManager.swift
//  view
//
//  Created by Devin Lehmacher on 3/22/23.
//

import Foundation

/// Represents the  represents the base directory for a graph.
/// Offers a flyweight mechanism for retrieving Node model objects that represent nodes in the graph.
class Graph {
    let dir: URL
    let basePath: URL
    var maxNodeId: NID
    var cloudDocumentsPull: Task<Void, Never>?

    init?(dir: URL) async {
        self.dir = dir
        basePath = dir
        maxNodeId = NID.origin
        if !FileManager.default.fileExists(atPath: metaPath(for: NID.origin).path) {
            warn("couldn't find origin node meta data file")
            return nil
        }
        cloudDocumentsPull = await pullCloudDocuments()
    }

    func updateMaxNodeId(from url: URL) {
        let lastPathComponent = url.lastPathComponent
        if lastPathComponent.hasSuffix(".json"),
           let dotIx = lastPathComponent.lastIndex(of: ".") {
            let dotPrevIx = lastPathComponent.index(before: dotIx)
            if let id = Int(url.lastPathComponent[...dotPrevIx]),
               id > maxNodeId {
                maxNodeId = id
            }
        }
    }

    func refresh() async {
        cloudDocumentsPull?.cancel()
        cloudDocumentsPull = await pullCloudDocuments()
    }

    @MainActor
    func pullCloudDocuments() async -> Task<Void, Never> {
        let query = NSMetadataQuery()
        query.searchScopes = [NSMetadataQueryAccessibleUbiquitousExternalDocumentsScope, dir]
        query.predicate = NSPredicate(value: true)
        @Sendable func updateWithQueryResults() {
            query.disableUpdates()
            for result in query.results as! [NSMetadataItem] {
                let url = result.value(forAttribute: NSMetadataItemURLKey) as! URL
                updateMaxNodeId(from: url)
            }
            query.enableUpdates()
        }
        let semaphor = Semaphor(initialCount: 0)
        let task: Task<Void, Never> = Task {
            for await _ in NotificationCenter.default.notifications(named: .NSMetadataQueryDidFinishGathering) {
                info("received initial query results")
                updateWithQueryResults()
                await semaphor.signal()
                break
            }
            for await _ in NotificationCenter.default.notifications(named: .NSMetadataQueryDidUpdate) {
                info("received query result update")
                updateWithQueryResults()
            }
            info("completed (almost certainly because Task was cancelled)")
        }
        query.start()
        info("started query to fetch iCloud Document contents")
        await semaphor.wait()
        return task
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

    /// Creates a new node not connected to anything
    func createNewNode() -> Node {
        let newNodeId = maxNodeId + 1
        let newMeta = NodeMeta(id: newNodeId, incoming: [:], outgoing: [:])
        guard let data: Data = try? JSONEncoder().encode(newMeta) else {
            error("failed to encode JSON for NodeMeta")
            fatalError("couldn't create a node")
        }
        guard FileManager.default.createFile(atPath: metaPath(for: newNodeId).path, contents: data) else {
            error("failed to create file for new node")
            fatalError("failed to create a file")
        }
        guard let node = self[newNodeId] else {
            error("couldn't access newly created node")
            fatalError("couldn't create a node")
        }
        maxNodeId = max(newNodeId, maxNodeId)
        return node
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
