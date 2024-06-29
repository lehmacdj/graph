//
//  AsyncSequenceManager.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/25/24.
//

import Foundation
import SwiftUI
import AsyncAlgorithms

func makeAsyncStream(id: Int, max: Int) -> AsyncStream<String> {
    var current = max
    return AsyncStream {
        guard current >= 0 else { return nil }
        current -= 1
        return "stream \(id), value \(current + 1)"
    }
}


// proof of concept for how to subscribe to several child streams using swift concurrency
func test() async {
    let channel = AsyncChannel<String>()
    await withTaskGroup(of: Void.self) { group in
        group.addTask {
            for await item in makeAsyncStream(id: 0, max: 5) {
                await channel.send(item)
            }
        }
        group.addTask {
            for await item in makeAsyncStream(id: 1, max: 5) {
                await channel.send(item)
            }
        }
        for await item in channel {
            if item == "stream 1, value 2" {
                group.addTask {
                    for await item in makeAsyncStream(id: 2, max: 5) {
                        await channel.send(item)
                    }
                }
            }
            print(item)
        }
    }
}

#Preview {
    Text("hello")
        .task(test)
}
