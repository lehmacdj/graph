//
//  Dictionary+Frequencies.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/26/25.
//

extension Sequence where Element: Hashable {
    func frequencies() -> [Element: Int] {
        var result = [Element: Int]()
        for element in self {
            result[element] = (result[element] ?? 0) + 1
        }
        return result
    }
}
