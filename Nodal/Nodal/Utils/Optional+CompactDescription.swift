//
//  Optional+CompactDescription.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/26/25.
//

extension Optional {
    var compactDescription: String {
        switch self {
        case .none:
            return "nil"
        case .some(let value):
            return String(describing: value)
        }
    }
}
