//
//  FilePresenterManager.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/31/25.
//

import Foundation

actor FilePresenterManager {
    private var managedPresenters = [any NSFilePresenter]()
    private var isActive = false

    func add(_ presenter: NSFilePresenter) {
        managedPresenters.append(presenter)
        if isActive {
            NSFileCoordinator.addFilePresenter(presenter)
        }
    }

    func remove(_ presenter: NSFilePresenter) {
        if let ix = managedPresenters.firstIndex(where: { $0 === presenter }) {
            managedPresenters.remove(at: ix)
            if isActive {
                NSFileCoordinator.removeFilePresenter(presenter)
            }
        } else {
            logWarn("trying to remove a presenter that wasn't added")
        }
    }

    func background() {
        assert(isActive, "already backgrounded")
        logInfo("backgrounding")
        isActive = false
        for presenter in managedPresenters {
            NSFileCoordinator.removeFilePresenter(presenter)
        }
    }

    func unbackground() {
        assert(!isActive, "already active")
        logInfo("foregrounding")
        isActive = true
        for presenter in managedPresenters {
            NSFileCoordinator.addFilePresenter(presenter)
        }
    }
}
