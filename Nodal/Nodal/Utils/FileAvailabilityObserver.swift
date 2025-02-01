//
//  FileAvailabilityObserver.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/12/25.
//

import Foundation
import Combine

enum FileAvailability {
    case noData
    case availableRemotely
    case availableLocally
}

/// It might be possible to replace this with just ``DataDocument`` if we open it only when needed and use `observedPresentedItemUbiquityAttributes`
/// https://developer.apple.com/documentation/foundation/nsfilepresenter/2909022-observedpresenteditemubiquityatt
actor FileAvailabilityObserver {
    let url: URL
    private(set) var availability: FileAvailability?
    private var updatesContinuations = [UUID: AsyncStream<FileAvailability>.Continuation]()

    init(url: URL, refreshInterval: Duration = .milliseconds(500)) {
        self.url = url
        Task { [weak self] in
            while !Task.isCancelled && self != nil {
                await self?.updateAvailability()
                try await Task.sleep(for: refreshInterval)
            }
        }
    }

    func updateAvailability() {
        do {
            let resourceValues = try url.resourceValues(forKeys: [.ubiquitousItemDownloadingStatusKey])
            switch resourceValues.ubiquitousItemDownloadingStatus {
            case nil:
                availability = .noData
            case .notDownloaded:
                availability = .availableRemotely
            case .downloaded:
                availability = .availableLocally
            case .current:
                availability = .availableLocally
            case .some(let status):
                logError("unexpected download status \(status)")
            }
        } catch {
            logError(error)
        }
        updatesContinuations.values.forEach {
            if let availability {
                $0.yield(availability)
            }
        }
    }

    private func removeUpdatesContinuation(_ uuid: UUID) {
        updatesContinuations.removeValue(forKey: uuid)
    }

    var updates: some AsyncSequence<DataAvailability, Never> & Sendable {
        let (updates, updatesContinuation) = AsyncStream<FileAvailability>.makeStream()
        let uuid = UUID()
        updatesContinuation.onTermination = { [weak self] _ in
            Task {
                await self?.removeUpdatesContinuation(uuid)
            }
        }
        updatesContinuations[uuid] = updatesContinuation
        if let availability {
            updatesContinuation.yield(availability)
        }
        return updates
    }

    deinit {
        updatesContinuations.values.forEach { $0.finish() }
    }
}
