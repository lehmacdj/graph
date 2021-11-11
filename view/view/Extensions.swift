//
//  Extensions.swift
//  view
//
//  Created by Devin Lehmacher on 10/10/21.
//

import Foundation

extension String: Identifiable {
    public var id: String { self }
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

extension Sequence {
    public func sorted<T>(
        on projection: (Element) -> T
    ) -> [Element] where T: Comparable {
        return self.sorted(by: { x, y in projection(x) < projection(y) })
    }
}
