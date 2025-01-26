//
//  NodeMetadataDocument.swift
//  Nodal
//
//  Created by Devin Lehmacher on 12/2/23.
//

import Combine
import UIKit
import UniformTypeIdentifiers

/// There should only be a maximum of one of these per NID
final class NodeMetadataDocument: SwiftConcurrencyUIDocument {
    struct FailedToOpenDocument: Error {
        let documentClosed: Bool
        let inConflict: Bool
        let savingError: Bool
        let editingDisabled: Bool
        let progressAvailable: Bool

        init(documentState: UIDocument.State) {
            documentClosed = documentState.contains(.closed)
            inConflict = documentState.contains(.inConflict)
            savingError = documentState.contains(.savingError)
            editingDisabled = documentState.contains(.editingDisabled)
            progressAvailable = documentState.contains(.progressAvailable)
        }
    }

    init(metaURL: URL) async throws {
        super.init(fileURL: metaURL)
        guard await open() else {
            throw FailedToOpenDocument(documentState: documentState)
        }
    }

    /// Initialized with initially invalid information, but we read before returning from the constructor so it's never invalid to an outside observer
    /// TODO: add error to publisher here too to account for scenarios involving node deletions
    @Published private var _meta: Node<NoAugmentation>?
    var meta: Node<NoAugmentation> {
        get {
            guard let _meta else {
                fatalError("initialized in init")
            }
            return _meta
        }
        set {
            _meta = newValue
        }
    }


    var metaPublisher: AnyPublisher<Node<NoAugmentation>, Never> {
        return $_meta
            .compactMap { $0 }
            .subscribe(on: DispatchQueue.main)
            .eraseToAnyPublisher()
    }

    struct InvalidFileType: Error {}

    override func contents(forType typeName: String) throws -> Any {
        guard typeName == UTType.json.identifier else {
            throw InvalidFileType()
        }
        let encoder = JSONEncoder()
        return try encoder.encode(meta)
    }

    struct ContentsIsNotData: Error {}

    override func load(fromContents contents: Any, ofType typeName: String?) throws {
        guard typeName == UTType.json.identifier else {
            logError("invalid file type")
            throw InvalidFileType()
        }
        guard let data = contents as? Data else {
            logError("contents is not data")
            throw ContentsIsNotData()
        }
        do {
            meta = try JSONDecoder().decode(Node<NoAugmentation>.self, from: data)
        } catch {
            logError(error)
            throw error
        }
    }
}
