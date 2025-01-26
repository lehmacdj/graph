//
//  SwiftConcurrencyUIDocument.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/26/25.
//

import UIKit

/// UIDocument that uses Swift Concurrency's executors for serialization of file access tasks
/// Without this, when opening/writing to many `UIDocument`s at the same time it is possible for GCD to run out of threads
class SwiftConcurrencyUIDocument: UIDocument {
    private let queueContinuation: AsyncStream<UnsafeTransfer<() -> Void>>.Continuation
    private let queueExecutorTask: Task<Void, Never>

    override init(fileURL url: URL) {
        let (queue, queueContinuation) = AsyncStream<UnsafeTransfer<() -> Void>>.makeStream()
        self.queueContinuation = queueContinuation
        queueExecutorTask = Task.detached(priority: .background) { [queue] in
            for await task in queue {
                task.unsafelySending()
            }
        }
        super.init(fileURL: url)
    }

    deinit {
        queueContinuation.finish()
        queueExecutorTask.cancel()
    }

    override func performAsynchronousFileAccess(_ block: @escaping () -> Void) {
        let sendableBlock = UnsafeTransfer(unsafelySending: block)
        queueContinuation.yield(sendableBlock)
    }

    /// For some reason overriding this function prevents a crash where "close" is interpreted as being called on the wrong thread
    /// I'm guessing the explicit override forces a context switch the MainActor?
    override func close() async -> Bool {
        await super.close()
    }
}
