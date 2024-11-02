//
//  GraphManager.swift
//  Nodal
//
//  Created by Devin Lehmacher on 3/22/23.
//

import Combine
import Foundation

struct SpecialNodeDoesNotExist: LocalizedError {}

/// Represents the  represents the base directory for a graph.
/// Offers a flyweight mechanism for retrieving Node model objects that represent nodes in the graph.
actor GraphManager<N: Node> {
    let basePath: URL

    private var directoryObserver: DirectoryObserver?

    private var _graphChanges: AnyPublisher<GraphChange, Never>?

    private func initSystemNodes() async throws {
        for node in SystemNodes.allCases where (try? await self[node.nid]) == nil {
            let _ = try await createNode(withNid: node.nid)
        }
    }

    private func initDirectoryObserverAndGraphChanges() {
        let directoryObserver = DirectoryObserver(url: basePath)
        let graphChanges: any Publisher<GraphChange, Never> = directoryObserver.changes.compactMap { [weak self] change in
            switch (change, self?.changeType(for: change.url))  {
            case (.added, .metadata(let nid)):
                return .metadataAdded(nid)
            case (.removed, .metadata(let nid)):
                return .metadataRemoved(nid)
            case (.changed, .metadata(let nid)):
                return .metadataUpdated(nid)
            case (.added, .data(let nid)):
                return .dataAdded(nid)
            case (.removed, .data(let nid)):
                return .dataRemoved(nid)
            case (.changed, .data(let nid)):
                return .dataUpdated(nid)
            default:
                return nil
            }
        }
        self.directoryObserver = directoryObserver
        self._graphChanges = graphChanges.eraseToAnyPublisher()
    }

    init(dir: URL) async throws {
        basePath = dir
        try await initSystemNodes()
        initDirectoryObserverAndGraphChanges()
    }

    nonisolated func metaPath(for nid: NID) -> URL {
        basePath.appendingPathComponent(nid.metaPath)
    }

    nonisolated func dataPath(for nid: NID) -> URL {
        basePath.appendingPathComponent(nid.dataPath)
    }

    subscript(id: NID) -> N {
        get async throws {
            return try await N(nid: id, root: self)
        }
    }

    var origin: N {
        get async {
            let origin: N
            do {
                origin = try await self[NID.origin]
            } catch {
                logError("origin node doesn't exist: \(error)")
                fatalError("origin node doesn't exist: \(error)")
            }
            return origin
        }
    }

    var tags: Tags<N>? {
        get async {
            guard let tags = try? await self[NID.tags],
                  let tagIncoming = tags.meta.incoming["tags"],
                  tagIncoming.contains(NID.origin) else {
                logWarn("no tags node found")
                return nil
            }
            return Tags(tagNode: tags, root: self)
        }
    }

    private enum ChangeType {
        case metadata(NID)
        case data(NID)
    }

    private nonisolated func changeType(for url: URL) -> ChangeType? {
        let suffix = url.trimmingPath(prefix: basePath)

        if let nid = suffix?.trimming(suffix: ".json").flatMap(NID.init(representation:)) {
            return .metadata(nid)
        } else if let nid = suffix?.trimming(suffix: ".data").flatMap(NID.init(representation:)) {
            return .data(nid)
        } else {
            return nil
        }
    }

    enum GraphChange {
        case metadataRemoved(NID)
        case metadataAdded(NID)
        case metadataUpdated(NID)
        case dataRemoved(NID)
        case dataAdded(NID)
        case dataUpdated(NID)
    }

    var graphChanges: AnyPublisher<GraphChange, Never> {
        guard let _graphChanges else {
            fatalError("graphChanges is nil unexpectedly, this should be impossible post initialization")
        }
        return _graphChanges
    }

    enum DataChange {
        case added(NID)
        case changed(NID)
        case removed(NID)
    }

    func dataChanges(for nid: NID) -> AnyPublisher<DataChange, Never> {
        graphChanges.compactMap { change in
            switch change {
            case .dataAdded(let id) where id == nid:
                return .added(id)
            case .dataUpdated(let id) where id == nid:
                return .changed(id)
            case .dataRemoved(let id) where id == nid:
                return .removed(id)
            default:
                return nil
            }
        }.eraseToAnyPublisher()
    }

    func createNode(withNid nid: NID) async throws -> N {
        let newMeta = NodeMeta(id: nid, incoming: [:], outgoing: [:])
        let data: Data = try JSONEncoder().encode(newMeta)
        guard FileManager.default.createFile(atPath: metaPath(for: nid).path, contents: data) else {
            throw CouldNotCreateFile(path: metaPath(for: nid))
        }
        return try await self[nid]
    }

    /// Creates a new node not connected to anything
    func createNewNode() async throws -> N {
        try await createNode(withNid: NID.random())
    }

    func addLink(from start: NID, to end: NID, via transition: String) async {
        guard let start = try? await self[start],
              let end = try? await self[end] else {
            return
        }

        addLink(from: start, to: end, via: transition)
    }

    func addLink(from startNode: N, to end: NID, via transition: String) async {
        guard let endNode = try? await self[end] else {
            return
        }

        addLink(from: startNode, to: endNode, via: transition)
    }

    func addLink(from startNode: N, to endNode: N, via transition: String) {
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

    func removeLink(from start: NID, to end: NID, via transition: String) async {
        guard let start = try? await self[start],
              let end = try? await self[end] else {
            return
        }

        removeLink(from: start, to: end, via: transition)
    }

    func removeLink(from startNode: N, to end: NID, via transition: String) async {
        guard let endNode = try? await self[end] else {
            return
        }

        removeLink(from: startNode, to: endNode, via: transition)
    }

    func removeLink(from startNode: N, to endNode: N, via transition: String) {
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

    func forceRemove(nid: NID) async {
        guard let node = try? await self[nid] else {
            logWarn("tried to force remove a node that we couldn't access")
            return
        }

        await forceRemove(node: node)
    }

    func forceRemove(node: N) async {
        for parent in node.incoming {
            if let parentNode = try? await self[parent.nid] {
                var parentMeta = parentNode.meta
                parentMeta.outgoing[parent.transition] = parentMeta.outgoing[parent.transition]?.removing(node.nid)
                parentNode.meta = parentMeta
            } else {
                logWarn("parent (\(parent.nid)) of \(node.nid) is missing")
            }
        }

        for child in node.outgoing {
            if let childNode = try? await self[child.nid] {
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
