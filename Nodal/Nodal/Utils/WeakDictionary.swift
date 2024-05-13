//
//  WeakDictionary.swift
//  Nodal
//
//  Created by Devin Lehmacher on 9/10/23.
//

import Foundation

private struct WeakWrapper<T: AnyObject> {
    weak var reference: T?
}

/// Threshold for when to compact the dictionary. A higher number means compaction happens less often. Generally this should be irrelevant because the operations skip references that are missing
private let weakDictionaryCompactThreshold = 1

/// A wrapper around a dictionary that stores it's values as weak references
/// Most operations compact the underlying dictionary getting rid of stale references
/// Ideal for implementation of cache's that don't want to hold strong references
struct WeakDictionary<Key: Hashable, T: AnyObject> {
    private var backingDictionary = [Key:WeakWrapper<T>]()

    /// A number which increases triggering compactification after reaching `compactThreshold` then resetting
    private var compactTimer: Int = 0

    private mutating func amortizedCompact() {
        compactTimer += 1
        if compactTimer == weakDictionaryCompactThreshold {
            compact()
        }
    }

    private mutating func compact() {
        backingDictionary = backingDictionary.filter { $0.value.reference != nil }
        compactTimer = 0
    }

    var count: Int {
        mutating get {
            // we need to compact for the count to be accurate
            compact()
            return backingDictionary.count
        }
    }

    subscript(_ key: Key) -> T? {
        mutating get {
            amortizedCompact()
            return backingDictionary[key]?.reference
        }
        mutating set {
            backingDictionary[key] = WeakWrapper(reference: newValue)
            amortizedCompact()
        }
    }
}
