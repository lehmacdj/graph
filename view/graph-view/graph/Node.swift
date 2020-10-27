//
//  Node.swift
//  graph-view
//
//  Created by Devin Lehmacher on 9/8/19.
//  Copyright © 2019 Devin Lehmacher. All rights reserved.
//

import Foundation

typealias NID = Int64

struct Node  {
    let nid: NID
    let incoming: [NID:NID] // transition -> target
    let outgoing: [NID:NID] // transition -> source
}
