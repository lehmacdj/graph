//
//  NodalApp.swift
//  Nodal
//
//  Created by Devin Lehmacher on 10/10/21.
//

import SwiftUI

@main
struct NodalApp: App {
    var body: some Scene {
        FilePresenterBackgroundingScene()
    }
}

struct FilePresenterBackgroundingScene: Scene {
    @Environment(\.scenePhase) var scenePhase
    @Environment(\.filePresenterManager) var filePresenterManager

    var body: some Scene {
        WindowGroup {
            FileSelectorScreen()
        }
        .onChange(of: scenePhase) { _, newPhase in
            switch newPhase {
            case .active, .inactive:
                Task { await filePresenterManager.unbackground() }
            case .background:
                Task { await filePresenterManager.background() }
            @unknown default:
                logWarn("unknown new scene phase")
            }
        }
    }
}
