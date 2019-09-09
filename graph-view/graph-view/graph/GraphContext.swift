//
//  GraphContext.swift
//  graph-view
//
//  Created by Devin Lehmacher on 9/8/19.
//  Copyright © 2019 Devin Lehmacher. All rights reserved.
//

import Foundation

struct GraphContext {
    let graphPath: String
}

extension GraphContext {
    func readNode(nid: NID) -> Node {
        return Node(nid: nid, incoming: <#T##[NID : NID]#>, outgoing: <#T##[NID : NID]#>)
    }
}
