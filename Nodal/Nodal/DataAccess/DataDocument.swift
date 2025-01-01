//
//  DataDocument.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/18/23.
//

import UIKit

final class DataDocument: UIDocument {
    init?<N: Node>(node: N) async {
        guard let dataURL = node.dataURL else {
            return nil
        }
        super.init(fileURL: dataURL)
        guard await super.open() else {
            return nil
        }
    }

    var data = Data()

    override func contents(forType typeName: String) throws -> Any {
        data
    }

    struct ContentsIsNotData: Error {}

    override func load(fromContents contents: Any, ofType typeName: String?) throws {
        guard let data = contents as? Data else {
            throw ContentsIsNotData()
        }
        self.data = data
    }
}
