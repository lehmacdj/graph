//
//  Node.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/18/23.
//

import Combine
import Foundation

typealias DefaultNode = DocumentNode

protocol Node: AnyObject, ObservableObject {
    init(nid: NID, root: GraphManager<Self>) async throws

    /// The id of this node
    var nid: NID { get }

    /// The metadata for this node
    var meta: NodeMeta { get set }
    var metaPublisher: AnyPublisher<NodeMeta, Never> { get }

    /// Get all nodes that this node has outgoing transitions to
    var outgoing: [NodeTransition] { get }

    /// Get all nodes that have incoming transitions to this node
    var incoming: [NodeTransition] { get }

    /// Get the nodes that are linked by a transition from this node
    subscript(transition: String) -> [Self] { get async }

    // MARK: tags

    /// Get the set of tags for this node. Defaults to empty set if unable to get tags (this is sus)
    var tags: Set<String> { get async }

    /// Set all of the tags for the node
    func set(tags: Set<String>) async

    /// Toggle one tag for the node
    func toggle(tag: String) async

    // MARK: managing node favorites/worse

    /// Get the favorites node if it exists
    var favorites: Self? { get async }

    /// Get the favorites node creating a new child if it does not
    func favorites() async -> Self

    func isFavorite(child: NID) async -> Bool

    func toggleFavorite(child: NID) async

    /// Get the worse node if it exists
    var worse: Self? { get async }

    /// Get the worse node creating a new child if it does not
    func worse() async -> Self

    func isWorse(child: NID) async -> Bool

    func toggleWorse(child: NID) async

    // MARK: managing node associated data

    var dataURL: URL? { get }

    /// whether the data requires a download from iCloud to be accessed
    /// this is for avoiding downloading stuff when it would just be used to
    /// display a thumbnail
    var dataRequiresDownload: Bool { get }

    var data: Data? { get async }

    func deleteMetaAndData()
}

protocol GraphManagerNode: Node {
    var manager: GraphManager<Self> { get }
}

extension Node {
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

    /// Utility function likely to be useful to implementations of Node
    func links(to nid: NID) -> [String] {
        var result = [String]()
        for (transition, targets) in self.meta.outgoing {
            if targets.contains(nid) {
                result.append(transition)
            }
        }
        return result
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

    func isFavorite(child: NID) async -> Bool {
        return !(await favorites?.links(to: child).isEmpty ?? true)
    }

    func isWorse(child: NID) async -> Bool {
        return !(await worse?.links(to: child).isEmpty ?? true)
    }
}

extension GraphManagerNode {
    subscript(transition: String) -> [Self] {
        get async {
            guard let ids = meta.outgoing[transition] else {
                // debug because sometimes we use this to check if an optional transition
                // exists, and the log message is too noisy to be a warning
                logDebug("didn't find transition \(transition) from node \(meta.id)")
                return []
            }
            return await Array(ids.async.compactMap { try? await self.manager[$0] })
        }
    }

    var tags: Set<String> {
        get async {
            guard let tags = await manager.tags else {
                return Set()
            }
            let tagContainers = Set(
                meta.incoming
                    .filter { !$0.value.intersection(tags.tagNids).isEmpty }
                    .values
                    .flatMap { $0 })
            var result = Set<String>()
            for tagContainer in tagContainers {
                if let tagContainer = try? await manager[tagContainer] {
                    for tag in tagContainer.meta.incoming.filter({ $0.value.contains(NID.tags) }).keys {
                        result.insert(tag)
                    }
                }
            }
            return result
        }
    }

    func set(tags newTags: Set<String>) async {
        guard let tags = await manager.tags else {
            logWarn("trying to set tags, but there is no tags node")
            return
        }

        let oldTags = await self.tags

        await tags.modify(
            for: self,
            adding: newTags.subtracting(oldTags),
            removing: oldTags.subtracting(newTags))
    }

    func createNewChild(via transition: String) async -> Self {
        let node = await manager.createNewNode()
        await manager.addLink(from: self, to: node, via: transition)
        return node
    }

    var favorites: Self? {
        get async {
            await self["favorites"].first
        }
    }

    func favorites() async -> Self {
        if let favorites = await favorites {
            return favorites
        }

        return await createNewChild(via: "favorites")
    }

    var worse: Self? {
        get async {
            await self["worse"].first
        }
    }

    func worse() async -> Self {
        if let worse = await worse {
            return worse
        }

        return await createNewChild(via: "worse")
    }

}

extension GraphManagerNode where ObjectWillChangePublisher == ObservableObjectPublisher {
    func toggleFavorite(child childNID: NID) async {
        if let favoriteLinks = await favorites?.links(to: childNID).nilIfEmpty() {
            if let favorites = await favorites {
                // if favorites doesn't exist, no need to remove from favorites node either
                for favoriteLink in favoriteLinks {
                    await manager.removeLink(from: favorites, to: childNID, via: favoriteLink)
                }
                logInfo("unfavorited node")
            } else {
                logWarn("not unfavoriting node because favorites node doesn't exist")
            }
        } else {
            objectWillChange.send()
            await manager.addLink(from: await favorites(), to: childNID, via: "")
            logInfo("successfully favorited node")
        }
    }

    func toggleWorse(child childNID: NID) async {
        if let worseLinks = await worse?.links(to: childNID).nilIfEmpty() {
            if let worse = await worse {
                objectWillChange.send()
                for worseLink in worseLinks {
                    await manager.removeLink(from: worse, to: childNID, via: worseLink)
                }
            } else {
                logWarn("not unworsening node because worse node doesn't exist")
            }
        } else {
            objectWillChange.send()
            await manager.addLink(from: await worse(), to: childNID, via: "")
            logInfo("successfully worsened node")
        }
    }
}
