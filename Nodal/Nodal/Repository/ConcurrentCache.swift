//
//  ConcurrentCache.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/4/25.
//

import Combine
import Foundation

/// A cache that manages some object that can be initialized / deallocated using async methods.
actor ConcurrentCache<K: Hashable & Sendable, V: Sendable> {
    private let create: (K) async throws -> V
    private let destroy: (K, V) async -> Void
    let logContext: [String]

    init(
        create: @escaping (K) async throws -> V,
        destroy: @escaping (K, V) async -> Void,
        logContext: String? = nil
    ) {
        self.create = create
        self.destroy = destroy
        self.logContext = logContext.map { [$0] } ?? []
    }

    private var storage: [K: CacheEntry] = [:]

    private enum CacheEntry {
        /// If there are concurrent attempts to open a particular entry all but the first may have to wait for the first attempt to complete
        case creating(waiters: [CheckedContinuation<Void, Never>] = [])
        case live(referenceCount: Int, value: V)
        /// If there are concurrent attempts to open a particular entry all but the first may have to wait for the first attempt to complete
        case destroying(waiters: [CheckedContinuation<Void, Never>] = [])

        var value: V? {
            switch self {
            case .live(_, let document): document
            case .creating, .destroying: nil
            }
        }

        var referenceCount: Int? {
            switch self {
            case .live(let referenceCount, _): referenceCount
            case .destroying, .creating: nil
            }
        }

        mutating func decrementReferenceCount() {
            guard case .live(let referenceCount, let value) = self else {
                fatalError("can't change reference count if not live")
            }
            precondition(referenceCount > 0)
            self = .live(referenceCount: referenceCount - 1, value: value)
        }

        mutating func incrementReferenceCount() {
            guard case .live(let referenceCount, let value) = self else {
                fatalError("can't change reference count if not live")
            }
            self = .live(referenceCount: referenceCount + 1, value: value)
        }

        mutating func addWaiter(_ continuation: CheckedContinuation<Void, Never>) {
            switch self {
            case .live:
                fatalError("invalid to add a waiter when live")
            case .creating(let waiters):
                self = .creating(waiters: waiters + [continuation])
            case .destroying(let waiters):
                self = .destroying(waiters: waiters + [continuation])
            }
        }
    }

    /// The returned anycancellable relinquishes the hold on the cache entry. Destroy will be called even if `V` is still retained and used somewhere else.
    func getOrCreate(_ key: K) async throws -> (AnyCancellable, V) {
        // ensure we have a live cache entry
        logDebug("getting or creating \(key)")
        while true {
            logDebug("top of loop \(key)")
            if case .live = storage[key] {
                // leave the loop, then in the guard we will have a live cache entry
                logDebug("breaking \(key)")
                break
            } else if var cacheEntry = storage[key] {
                logDebug("starting continuation \(key)")
                // wait for the closing or opening operation to complete then try again
                await withCheckedContinuation { continuation in
                    logDebug("adding continuation to waiters \(key)")
                    cacheEntry.addWaiter(continuation)
                    storage[key] = cacheEntry
                    logDebug("added continuation to waiters \(key)")
                }
            } else {
                // initialize a new cache entry
                logDebug("creating \(key)")
                storage[key] = .creating()
                do {
                    logInfo("allocating new cache entry \(key)")
                    let value = try await create(key)
                    logDebug("created new value \(key)")
                    guard case .creating(let waiters) = storage[key] else {
                        fatalError("only one opening operation can be active at a time")
                    }
                    logDebug("resuming waiters \(key)")
                    waiters.forEach { $0.resume() }
                    logDebug("marking entry as live \(key)")
                    storage[key]! = .live(referenceCount: 0, value: value)
                    // we probably could break/return here, but the logic is clearer if there
                    // is only a single point where it is possible to break out of the loop
                } catch {
                    logDebug("exception \(error) while creating new value \(key)")
                    storage.removeValue(forKey: key)
                    throw error
                }
            }
        }

        // safe because we checked above without suspend points in between
        logDebug("incrementing reference count \(key)")
        storage[key]!.incrementReferenceCount()
        let value = storage[key]!.value!

        logDebug("returning \(key)")
        return (AnyCancellable {
            logDebug("spawning task to remove reference \(key)")
            Task {
                logDebug("removing reference \(key)")
                await self.removeReference(to: key)
            }
        }, value)
    }

    private func removeReference(to key: K) async {
        logDebug("remove reference to \(key)")
        guard case .live = storage[key] else {
            // we only return a MetadataHandle when .live and only transition out of that
            // state in this method
            fatalError("cache was manipulated improperly")
        }

        storage[key]!.decrementReferenceCount()

        if storage[key]!.referenceCount! == 0 {
            let value = storage[key]!.value!
            storage[key]! = .destroying()
            logDebug("destroying cache entry \(key)")
            await destroy(key, value)
            guard case .destroying(let waiters) = storage[key]! else {
                fatalError("only one closing operation may happen at a time")
            }
            waiters.forEach { $0.resume() }
            storage.removeValue(forKey: key)
        }
    }
}
