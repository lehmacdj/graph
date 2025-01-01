//
//  Errors.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/14/23.
//

import Foundation

extension Optional {
    public struct OptionalNilError: Error {
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

struct CouldNotCreateFile: Error {
    let path: URL
}

