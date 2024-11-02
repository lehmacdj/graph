//
//  DirectoryObserver.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/11/23.
//

import UIKit
import Foundation
import Combine

class DirectoryObserver: NSObject, NSFilePresenter {
    var observedURL: URL

    private var applicationBackgroundedSubscription: Task<Void, Never>?
    private var applicationForegroundedSubscription: Task<Void, Never>?

    init(url: URL) {
        observedURL = url
        super.init()

        startObserving()

        // https://developer.apple.com/documentation/foundation/nsfilepresenter#1673868
        applicationBackgroundedSubscription = Task {
            for await _ in NotificationCenter.default.notifications(named: UIApplication.didEnterBackgroundNotification) {
                stopObserving()
            }
            logInfo("subscription to notification center cancelled")
        }
        applicationBackgroundedSubscription = Task {
            for await _ in NotificationCenter.default.notifications(named: UIApplication.willEnterForegroundNotification) {
                startObserving()
            }
            logInfo("subscription to notification center cancelled")
        }
    }

    deinit {
        stopObserving()
    }

    func startObserving() {
        NSFileCoordinator.addFilePresenter(self)
    }

    func stopObserving() {
        NSFileCoordinator.removeFilePresenter(self)
    }

    enum Change {
        case added(URL)
        case removed(URL)
        case changed(URL)

        var url: URL {
            switch self {
            case .added(let url), .removed(let url), .changed(let url):
                return url
            }
        }
    }

    var _changes = PassthroughSubject<Change, Never>()

    var changes: AnyPublisher<Change, Never> {
        _changes.eraseToAnyPublisher()
    }

    // MARK: - NSFilePresenter methods

    var presentedItemURL: URL? {
        return observedURL
    }

    var presentedItemOperationQueue = OperationQueue()

    func presentedSubitemDidAppear(at url: URL) {
        _changes.send(.added(url))
    }

    func presentedSubitemDidDisappear(at url: URL) {
        _changes.send(.removed(url))
    }

    func presentedSubitemDidChange(at url: URL) {
        _changes.send(.changed(url))
    }
}
