//
//  Errors.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/14/23.
//

import Foundation

/// A better default LocalizedError description for types that are codable
extension LocalizedError where Self: Codable {
    public var errorDescription: String? {
        try? String(data: JSONEncoder().encode(self), encoding: .utf8)
    }
}

extension Optional {
    public struct OptionalNilError: Codable, LocalizedError {
        let reason: String
        let file: String
        let callingFunction: String
        let line: Int
        let column: Int
    }

    func unwrapped(_ reason: String, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) throws -> Wrapped {
        if let self {
            return self
        } else {
            throw OptionalNilError(
                reason: reason,
                file: file,
                callingFunction: callingFunction,
                line: line,
                column: column
            )
        }
    }
}
