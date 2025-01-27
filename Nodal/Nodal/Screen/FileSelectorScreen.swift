//
//  FileSelectorView.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/7/23.
//

import SwiftUI

struct FileSelectorScreen: View {
    @State var fileUrl: URL? = nil

    @SceneStorage("fileUrlBookmark") var fileUrlBookmark: Data?

    // start out false so that we don't present the file importer and then immediately
    // hide it in the case that we have saved bookmark data
    @State var presentingFileImporter: Bool = false

    var body: some View {
        if let fileUrl = fileUrl {
            RootScreen(fileUrl: fileUrl, doSelectFile: initiateFileSelection)
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
            .onAppear {
                // set the fileUrl from a bookmark if we have one
                guard let fileUrlBookmark, fileUrl == nil else {
                    presentingFileImporter = true
                    return
                }

                var stale = false
                fileUrl = try? URL(resolvingBookmarkData: fileUrlBookmark, bookmarkDataIsStale: &stale)

                guard fileUrl?.startAccessingSecurityScopedResource() == true else {
                    logWarn("unable to start accessing fileUrl resolved from bookmark data")
                    presentingFileImporter = true
                    return
                }

                if stale {
                    self.fileUrlBookmark = try? fileUrl?.bookmarkData()
                }
            }
            .onChange(of: presentingFileImporter) {
                initiateFileSelection()
            }
            .fileImporter(
                isPresented: $presentingFileImporter,
                allowedContentTypes: [.folder],
                onCompletion: setFileURL
            )
        }
    }

    func initiateFileSelection() {
        fileUrl = nil
        fileUrlBookmark = nil
        presentingFileImporter = true
    }

    func setFileURL(_ result: Result<URL, Error>) {
        switch result {
        case .success(let url):
            guard url.startAccessingSecurityScopedResource() else {
                logWarn("wasn't granted access to \(url)")
                return
            }

            logInfo("successfully got access to \(url)")
            fileUrlBookmark = try? url.bookmarkData()

            if let oldUrl = fileUrl {
                oldUrl.stopAccessingSecurityScopedResource()
            }

            fileUrl = url
        case .failure(let error):
            logError(error)
            fileUrl = nil
            presentingFileImporter = true
        }
    }
}

#if DEBUG
#Preview {
    FileSelectorScreen()
}
#endif
