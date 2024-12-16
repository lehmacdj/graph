//
//  URL+GraphPaths.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/30/24.
//

import Foundation

extension URL {
    func appending(metaPathFor nid: NID) -> Self {
        self.appendingPathComponent(nid.metaPath)
    }

    func appending(dataPathFor nid: NID) -> Self {
        self.appendingPathComponent(nid.dataPath)
    }
}
