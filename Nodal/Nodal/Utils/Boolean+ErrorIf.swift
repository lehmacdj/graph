//
//  Boolean+ErrorIf.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/26/25.
//

extension Bool {
    struct BoolFalse: Error {
        let reason: String
    }
    
    func errorIfFalse(reason: String) throws(BoolFalse) {
        if !self {
            throw BoolFalse(reason: reason)
        }
    }
}
