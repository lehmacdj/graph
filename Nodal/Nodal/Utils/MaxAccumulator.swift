//
//  MaxAccumulator.swift
//  Nodal
//
//  Created by Devin Lehmacher on 12/28/24.
//

/// Like a set that only keeps track of the maximum value inserted into it.
struct Max<T> where T: Comparable {
    var max: T?

    mutating func insert(_ value: T) {
        if let max, max > value {
            return
        } else {
            max = value
        }
    }

    mutating func reset() {
        max = nil
    }
}
