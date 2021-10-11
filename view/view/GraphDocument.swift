//
//  viewDocument.swift
//  view
//
//  Created by Devin Lehmacher on 10/10/21.
//

import SwiftUI
import UniformTypeIdentifiers

extension UTType {
    static var graph: UTType {
        UTType(importedAs: "io.github.lehmacdj.graph")
    }
}

struct GraphDocument: FileDocument {
    var root: Root

    static var readableContentTypes: [UTType] { [.graph] }

    init(configuration: ReadConfiguration) throws {
        guard configuration.file.isDirectory
        else {
            throw CocoaError(.fileReadCorruptFile)
        }
        root = Root(dir: configuration.file)
    }
    
    func fileWrapper(configuration: WriteConfiguration) throws -> FileWrapper {
        throw CocoaError(.fileWriteVolumeReadOnly)
    }
}
