//
//  DataDocument.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/18/23.
//

import UIKit
import Combine

final class DataDocument: UIDocument {
    struct NoDataURL: Error {}

    convenience init<N: Node>(node: N) async throws {
        guard let dataURL = node.dataURL else {
            throw NoDataURL()
        }
        try await self.init(dataURL: dataURL)
    }

    struct FailedToOpenDocument: Error {}

    init(dataURL: URL) async throws {
        super.init(fileURL: dataURL)
        guard await super.open() else {
            throw FailedToOpenDocument()
        }
    }

    var data = Data() {
        willSet {
            _dataPublisher.send(newValue)
        }
    }

    var _dataPublisher = CurrentValueSubject<Data, Error>(Data())

    // TODO: throw error when closing
    var dataPublisher: AnyPublisher<Data, Error> {
        _dataPublisher.eraseToAnyPublisher()
    }

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

    struct DocumentClosedError: Error {}

    override func close() async -> Bool {
        _dataPublisher.send(completion: .failure(DocumentClosedError()))
        return await super.close()
    }
}
