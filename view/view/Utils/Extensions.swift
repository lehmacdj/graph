//
//  Extensions.swift
//  view
//
//  Created by Devin Lehmacher on 10/10/21.
//

// This file contains utility functions that we want to keep even if unused.
// periphery:ignore:all

import Foundation

extension String: Identifiable {
    public var id: String { self }
}

extension Set {
    func inserting(_ newElement: Self.Element) -> Set {
        return mutate(self) { $0.insert(newElement) }
    }

    func removing(_ member: Self.Element) -> Set {
        return mutate(self) { $0.remove(member) }
    }
}

extension Dictionary where Value: Sequence {
    /// Append a sequence to a list stored at some value. If there isn't
    /// a present value, the sequence is converted to a list and stored.
    public mutating func appendSeq<S>(
        _ seq: S,
        toKey key: Key
    ) where S: Sequence, Value == [S.Element] {
        var newValue = self[key] ?? [S.Element]()
        newValue.append(contentsOf: seq)
        self[key] = newValue
    }
}

extension Collection {
    public func nilIfEmpty() -> Self? {
        if self.isEmpty {
            return nil
        } else {
            return self
        }
    }
}

extension Sequence {
    public func sorted<T>(
        on projection: (Element) -> T
    ) -> [Element] where T: Comparable {
        return self.sorted(by: { x, y in projection(x) < projection(y) })
    }
}

extension Array: Comparable where Array.Element: Comparable {
    public static func < (lhs: Array<Element>, rhs: Array<Element>) -> Bool {
        var i = 0
        while (i < lhs.count && i < rhs.count) {
            if lhs[i] < rhs[i] {
                return true
            } else if lhs[i] > rhs[i] {
                return false
            }
            i += 1
        }
        return lhs.count > rhs.count
    }
}

extension Int: Identifiable {
    public var id: some Hashable {
        self
    }
}
