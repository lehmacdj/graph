//
//  Task+sleepForever.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/31/25.
//

extension Task where Success == Never, Failure == Never {
    static func sleepForever() async throws {
        while !Task.isCancelled {
            try await Task.sleep(for: .seconds(1_000_000))
        }
        try Task.checkCancellation()
    }
}
