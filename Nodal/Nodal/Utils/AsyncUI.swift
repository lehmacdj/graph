//
//  AsyncUI.swift
//  Nodal
//
//  Created by Devin Lehmacher on 3/26/23.
//

import Foundation

/// perhaps worth factoring out Error so that we can provide Equatable when Error + T conform to Equatable
enum Loading<T> {
    case idle
    case loading
    case loaded(T)
    case failed(Error)

    var isLoaded: Bool {
        switch self {
        case .loaded: true
        default: false
        }
    }

    static func loading(_ value: T?) -> Loading<T> {
        if let value {
            .loaded(value)
        } else {
            .loading
        }
    }

    static func done(_ value: () async throws -> T) async -> Loading<T> {
        do {
            return .loaded(try await value())
        } catch {
            return .failed(error)
        }
    }

    static func equalityCheck(
        loadedCheck: @escaping (T, T) -> Bool,
        errorCheck: @escaping (Error, Error) -> Bool
    ) -> (Loading<T>, Loading<T>) -> Bool {
        { (lhs, rhs) in
            switch (lhs, rhs) {
            case (.idle, .idle): true
            case (.loading, .loading): true
            case (.loaded(let lhs), .loaded(let rhs)): loadedCheck(lhs, rhs)
            case (.failed(let lhs), .failed(let rhs)): errorCheck(lhs, rhs)
            default: false
            }
        }
    }
}

extension Loading: Identifiable where T: Identifiable {
    var id: some Hashable {
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
