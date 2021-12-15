//
//  viewApp.swift
//  view
//
//  Created by Devin Lehmacher on 10/10/21.
//

import SwiftUI

@main
struct viewApp: App {
    @State var fileUrl: URL? = nil

    @State var presentingFileImporter: Bool = true

    var body: some Scene {
        WindowGroup {
            if let fileUrl = fileUrl {
                ContentView(fileUrl: fileUrl, doSelectFile: initiateFileSelection)
            } else {
                Button("Please select a graph directory...", action: {
                    // TODO(iOS 16): check if this is still necessary
                    // Note: ideally we would not even need to use the button and just
                    // persistently present the .fileImporter here
                    // workaround error causing dismissing the the fileImporter sheet
                    // with a swipe causing presentingFileImporter to be stuck in on state
                    // https://stackoverflow.com/questions/69613669
                    if presentingFileImporter {
                        presentingFileImporter = false
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) {
                            presentingFileImporter = true
                        }
                    } else {
                        presentingFileImporter = true
                    }
                })
                .onAppear { presentingFileImporter = true }
                .onChange(of: presentingFileImporter) { _ in
                    initiateFileSelection()
                }
                .fileImporter(
                    isPresented: $presentingFileImporter,
                    allowedContentTypes: [.graph, .directory, .folder],
                    onCompletion: setFileURL
                )
            }
        }
    }

    func initiateFileSelection() {
        fileUrl = nil
        presentingFileImporter = true
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
            fileUrl = nil
            presentingFileImporter = true
        }
    }
}
