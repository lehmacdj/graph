//
//  AsyncUI.swift
//  view
//
//  Created by Devin Lehmacher on 3/26/23.
//

import Foundation

public enum Loading<T> {
    case idle
    case loading
    case loaded(T)
    case failed(Error)

    static func done(_ value: () async throws -> T) async -> Loading<T> {
        do {
            return .loaded(try await value())
        } catch {
            return .failed(error)
        }
    }
}

extension Loading: Identifiable where T: Identifiable {
    public var id: some Hashable {
        switch self {
        case .idle:
            return "idle"
        case .loading:
            return "loading"
        case .loaded(let id):
            return "loaded(\(id))"
        case .failed(let error):
            return "failed(\(error))"
        }
    }
}
