//
//  viewApp.swift
//  view
//
//  Created by Devin Lehmacher on 10/10/21.
//

import SwiftUI

@main
struct viewApp: App {
    @State var fileUrl: URL? = nil {
        didSet {
            switch fileUrl {
            case .some(_):
                presentingFileImporter = false
            case .none:
                presentingFileImporter = true
            }
        }
    }

    @State var presentingFileImporter: Bool = true

    var body: some Scene {
        WindowGroup {
            if let fileUrl = fileUrl {
                ContentView(fileUrl: fileUrl)
            } else {
                // TODO: this behavior is broken; but semi functional it would be good if I could
                // get the prompt to always appear if there isn't a fileUrl; but its good enough
                Button("Please select a graph directory...", action: { presentingFileImporter.toggle() })
                    .onTapGesture { presentingFileImporter = true }
                    .fileImporter(
                        isPresented: $presentingFileImporter,
                        allowedContentTypes: [.graph, .directory, .folder],
                        onCompletion: setFileURL
                    )
            }
        }
    }
    
    func setFileURL(_ result: Result<URL, Error>) {
        switch result {
        case .success(let url):
            guard url.startAccessingSecurityScopedResource() else {
                warn("wasn't granted access to \(url.path)")
                return
            }
            if let oldUrl = fileUrl {
                oldUrl.stopAccessingSecurityScopedResource()
            }
            fileUrl = url
        case .failure(let err):
            error(err.localizedDescription)
        }
    }
}
