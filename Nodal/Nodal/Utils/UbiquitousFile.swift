//
//  UbiquitousFile.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/13/23.
//

import AsyncAlgorithms
import Foundation

// periphery:ignore
struct UbiquitousFileAttributes {
    let isUbiquitous: Bool

    let isUploaded: Bool
    let isUploading: Bool
    let uploadingError: Error?

    enum DownloadingStatus {
        case downloaded
        case current
        case notDownloaded

        init?(rawValue: URLUbiquitousItemDownloadingStatus) {
            switch rawValue {
            case .current:
                self = .current
            case .downloaded:
                self = .downloaded
            case .notDownloaded:
                self = .notDownloaded
            default:
                return nil
            }
        }
    }

    let downloadingStatus: DownloadingStatus
    let downloadRequested: Bool
    let isDownloading: Bool
    let downloadingError: Error?

    init?(for url: URL) {
        guard let values = try? url.resourceValues(forKeys: Self.keys) else {
            logError("couldn't get values for keys from NSMetadataItem")
            return nil
        }

        self.isUbiquitous =  values.isUbiquitousItem ?? false
        self.isUploaded = values.ubiquitousItemIsUploaded ?? false
        self.isUploading = values.ubiquitousItemIsUploading ?? false
        self.uploadingError = values.ubiquitousItemUploadingError as Error?
        self.downloadingStatus = DownloadingStatus(rawValue: values.ubiquitousItemDownloadingStatus ?? .current) ?? .current
        self.downloadRequested = values.ubiquitousItemDownloadRequested ?? false
        self.isDownloading = values.ubiquitousItemIsDownloading ?? false
        self.downloadingError = values.ubiquitousItemDownloadingError as Error?
    }

    static let keys = Set<URLResourceKey>([
        .isUbiquitousItemKey,
        .ubiquitousItemHasUnresolvedConflictsKey,

        .ubiquitousItemIsUploadedKey,
        .ubiquitousItemIsUploadingKey,
        .ubiquitousItemUploadingErrorKey,

        .ubiquitousItemDownloadingStatusKey,
        .ubiquitousItemDownloadRequestedKey,
        .ubiquitousItemIsDownloadingKey,
        .ubiquitousItemDownloadingErrorKey,
    ])
}

// periphery:ignore
actor UbiquitousFile {
    let url: URL

    struct AttributeListenerState {
        let lastSentAttributeRevision: Int
        let channel: AsyncChannel<UbiquitousFileAttributes?>
    }

    /// The UUID is a key that the listener can use to remove the channel later
    private var attributeListeners = [UUID:AttributeListenerState]()

    /// Incrementing value used for making sure that we don't send out of order UbiquitousFileAttribute updates
    private var attributeRevision: Int = 0

    /// I want to eventually swap this for a more specific type that encodes invariants that are true, but I don't know if it's possible for a file to be downloading and uploading at the same time currently
    /// If this is nil then the file doesn't exist
    fileprivate(set) var attributes: UbiquitousFileAttributes? {
        didSet {
            attributeRevision += 1
        }
    }


    fileprivate func attributesChanged() async {
        self.attributes = UbiquitousFileAttributes(for: url)
        for (key, listener) in attributeListeners {
            if listener.lastSentAttributeRevision < attributeRevision {
                attributeListeners[key] = AttributeListenerState(lastSentAttributeRevision: attributeRevision, channel: listener.channel)
                await listener.channel.send(attributes)
            }
        }
    }

    fileprivate func changed() async {
        await attributesChanged()
    }

    private lazy var filePresenter: UbiquitousFilePresenter = {
        UbiquitousFilePresenter(parent: self)
    }()

    init(at url: URL) async {
        self.url = url
        self.attributes = UbiquitousFileAttributes(for: url)
        NSFileCoordinator.addFilePresenter(self.filePresenter)
    }

    struct FileNoLongerExists: Error {}

    func download() async throws {
        if attributes?.downloadingStatus == .current {
            return
        }

        let channel = AsyncChannel<UbiquitousFileAttributes?>()
        let key = UUID()
        attributeListeners[key] = AttributeListenerState(lastSentAttributeRevision: attributeRevision, channel: channel)
        defer {
            attributeListeners.removeValue(forKey: key)
        }
        try FileManager.default.startDownloadingUbiquitousItem(at: url)
        for await newAttributes in channel {
            guard let newAttributes else {
                throw FileNoLongerExists()
            }

            if newAttributes.downloadingStatus == .current {
                return
            } else if newAttributes.downloadingStatus == .downloaded {
                logWarn("downloaded but not current")
            }
        }
    }

    func read() async throws -> Data {
        try await download()
        return try Data(contentsOf: url)
    }

    struct TryingToWriteToNonCurrentFile: Error {}

    func write(_ data: Data) throws {
        guard attributes?.downloadingStatus == .current else {
            throw TryingToWriteToNonCurrentFile()
        }
        try data.write(to: url)
    }

    var exists: Bool {
        // if we got attributes for the file, then it exists
        attributes != nil
    }
}

private class UbiquitousFilePresenter: NSObject, NSFilePresenter {
    let parent: UbiquitousFile

    init(parent: UbiquitousFile) {
        self.parent = parent
    }

    var presentedItemURL: URL? {
        parent.url
    }

    var presentedItemOperationQueue: OperationQueue = {
        let queue = OperationQueue()
        queue.maxConcurrentOperationCount = 1
        return queue
    }()

    func presentedItemDidChangeUbiquityAttributes(_ attributes: Set<URLResourceKey>) {
        guard !attributes.intersection(UbiquitousFileAttributes.keys).isEmpty else {
            return
        }
        Task {
            await parent.attributesChanged()
        }
    }

    func presentedItemDidChange() {
        Task {
            await parent.changed()
        }
    }
}
