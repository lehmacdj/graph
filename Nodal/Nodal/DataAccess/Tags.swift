//
//  Tags.swift
//  Nodal
//
//  Created by Devin Lehmacher on 8/6/23.
//

import Foundation

struct Tags<N: Node> {
    let tagNode: N
    let root: GraphManager<N>

    var tagOptions: Set<String> {
        return Set(tagNode.meta.outgoing.keys)
    }

    var tagNids: Set<NID> {
        return Set(tagNode.meta.outgoing.values.flatMap { $0 })
    }

    func modify(for node: N, adding toAdd: Set<String>, removing toRemove: Set<String>) async {
        logDebug("node: \(node.nid), adding: \(toAdd), removing: \(toRemove)")
        assert(toAdd.intersection(toRemove).isEmpty, "can't add and remove the same tag")

        for tag in toAdd {
            let tagContainers = await tagNode[tag]
            if tagContainers.isEmpty {
                logWarn("can't create new node yet")
                continue
            } else if tagContainers.count > 1 {
                logWarn("more than one node with same transition as tag, skipping: \(tag)")
                continue
            }
            let tagContainer = tagContainers[0]
            await root.addLink(from: tagContainer, to: node, via: "")
        }

        for tag in toRemove {
            let tagContainers = await tagNode[tag]
            if tagContainers.isEmpty {
                logWarn("can't create new node yet")
                continue
            } else if tagContainers.count > 1 {
                logWarn("more than one node with same transition as tag, skipping: \(tag)")
                continue
            }
            let tagContainer = tagContainers[0]
            await root.removeLink(from: tagContainer, to: node, via: "")
        }
    }
}
