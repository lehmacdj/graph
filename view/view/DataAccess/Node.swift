//
//  Node.swift
//  view
//
//  Created by Devin Lehmacher on 3/26/23.
//

import Foundation

class Node: ObservableObject {
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
                try data.write(to: root.metaPath(for: nid), options: .atomic)
                metaChangeSource.resume()

                // by not changing _meta until after having written the file successfully
                // we ensure that the view represents the actual state of the world
                _meta = newValue
            } catch {
                logWarn("failed writing data for node to disk")
            }
        }
    }

    var outgoing: [NodeTransition] {
        meta
            .outgoing
            .flatMap { (transition, nids) in
                nids.map { nid in NodeTransition(transition: transition, nid: nid) }
            }
    }

    var incoming: [NodeTransition] {
        var result = [NodeTransition]()
        for (transition, nids) in meta.incoming {
            for nid in nids {
                result.append(NodeTransition(transition: transition, nid: nid))
            }
        }
        return result
    }

    subscript(transition: String) -> [Node] {
        get async {
            guard let ids = meta.outgoing[transition] else {
                // debug because sometimes we use this to check if an optional transition
                // exists, and the log message is too noisy to be a warning
                logDebug("didn't find transition \(transition) from node \(meta.id)")
                return []
            }
            return await Array(ids.async.compactMap { [weak self] in await self?.root[$0] })
        }
    }

    // TODO: it's sus that this always returns a set even if one doesn't exist
    // it would be good to more gracefully support graphs without tags, or not
    // support opening a graph that doesn't have them
    var tags: Set<String> {
        get async {
            guard let tags = await root.tags else {
                return Set()
            }
            let tagContainers = Set(
                meta.incoming
                    .filter { !$0.value.intersection(tags.tagNids).isEmpty }
                    .values
                    .flatMap { $0 })
            var result = Set<String>()
            for tagContainer in tagContainers {
                if let tagContainer = await root[tagContainer] {
                    for tag in tagContainer.meta.incoming.filter({ $0.value.contains(NID.tags) }).keys {
                        result.insert(tag)
                    }
                }
            }
            return result
        }
    }

    func set(tags newTags: Set<String>) async {
            guard let tags = await root.tags else {
                logWarn("trying to set tags, but there is no tags node")
                return
            }

            let oldTags = await self.tags

            await tags.modify(
                for: self,
                adding: newTags.subtracting(oldTags),
                removing: oldTags.subtracting(newTags))
    }

    func toggle(tag: String) async {
        var tags = await tags
        if tags.contains(tag) {
            tags.remove(tag)
            await set(tags: tags)
        } else {
            tags.insert(tag)
            await set(tags: tags)
        }
    }

    var favorites: Node? {
        get async {
            await self["favorites"].first
        }
    }

    func favorites() async -> Node {
        if let favorites = await favorites {
            return favorites
        }

        return await createNewChild(via: "favorites")
    }

    var worse: Node? {
        get async {
            await self["worse"].first
        }
    }

    func worse() async -> Node {
        if let worse = await worse {
            return worse
        }

        return await createNewChild(via: "worse")
    }

    private func createNewChild(via transition: String) async -> Node {
        let node = await root.createNewNode()
        await root.addLink(from: self, to: node, via: transition)
        return node
    }

    private func links(to nid: NID) -> [String] {
        var result = [String]()
        for (transition, targets) in self.meta.outgoing {
            if targets.contains(nid) {
                result.append(transition)
            }
        }
        return result
    }

    func isFavorite(child: NID) async -> Bool {
        return !(await favorites?.links(to: child).isEmpty ?? true)
    }

    func toggleFavorite(child: Node) async {
        if let favoriteLinks = await favorites?.links(to: child.nid).nilIfEmpty() {
            for favoriteLink in favoriteLinks {
                await root.removeLink(from: favorites(), to: child, via: favoriteLink)
            }
        } else {
            await root.addLink(from: await favorites(), to: child, via: "")
        }
        objectWillChange.send()
    }

    func isWorse(child: NID) async -> Bool {
        return !(await worse?.links(to: child).isEmpty ?? true)
    }

    func toggleWorse(child: Node) async {
        if let worseLinks = await worse?.links(to: child.nid).nilIfEmpty() {
            for worseLink in worseLinks {
                await root.removeLink(from: await worse(), to: child, via: worseLink)
            }
        } else {
            await root.addLink(from: await worse(), to: child, via: "")
        }
        objectWillChange.send()
    }

    // MARK: data

    var dataUrl: URL? {
        let dataUrl = root.dataPath(for: nid)
        return FileManager.default.fileExists(atPath: dataUrl.path) ? dataUrl : nil
    }

    var hasData: Bool {
        dataUrl == nil
    }

    var data: Data? {
        get async throws {
            guard let dataUrl else { return nil }
            let session = URLSession(configuration: .ephemeral)
            let (data, _) = try await session.data(from: dataUrl)
            return data
        }
    }

    func deleteMetaAndData() {
        let fileManager = FileManager()
        do {
            try fileManager.removeItem(at: root.metaPath(for: nid))
            if let dataUrl = dataUrl {
                try fileManager.removeItem(at: dataUrl)
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
    let root: GraphManager

    private let metaChangeSource: DispatchSourceFileSystemObject
    private let metaHandle: FileHandle

    /// Prefer initializing this via a factory method that constructs it from a static Node or from the Root directly
    init?(nid: NID, root: GraphManager) {
        self.nid = nid
        self.root = root

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
