//
//  Semaphor.swift
//  view
//
//  Created by Devin Lehmacher on 2/12/23.
//

import Foundation
import AsyncAlgorithms

public actor Semaphor {
    private var channel: AsyncChannel<()>
    private var count: Int
    private var waiting: Int

    public init(initialCount count: Int) {
        channel = AsyncChannel()
        self.count = count
        waiting = 0
    }

    public func signal() async {
        if waiting == 0 {
            count += 1
        } else {
            waiting -= 1
            await channel.send(())
        }
    }

    public func wait() async {
        if count > 0 {
            count -= 1
        } else {
            waiting += 1
            for await _ in channel {
                return
            }
        }
    }
}
