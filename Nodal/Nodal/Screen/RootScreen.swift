//
//  RootScreen.swift
//  Nodal
//
//  Created by Devin Lehmacher on 10/10/21.
//

import SwiftUI

struct RootScreen: View {
    let fileUrl: URL
    let doSelectFile: () -> ()

    @State private var graphRepository: Loading<GraphRepository> = .loading
    @State private var filePresenterManager = FilePresenterManager()

    @State private var path: [NavToNode] = []

    var body: some View {
        NavigationStack(path: $path) {
            HStack {
                switch graphRepository {
                case .idle:
                    Text("Idle")
                case .loading:
                    Text("Loading...")
                case .loaded(let graphRepository):
                    let nodeVM = GraphRepositoryNodeVM(nid: NID.origin, graphRepository: graphRepository)
                    NodeScreen(of: nodeVM.eraseToAnyNodeVM())
                case .failed(let error):
                    ErrorIndicator(for: error)
                }
            }
            .toolbar {
                ToolbarItem(placement: .navigationBarLeading) {
                    Button(action: doSelectFile) {
                        Text("Select graph")
                    }
                }
            }
            .navigationDestination(for: NavToNode.self) { nav in
                NodeScreen(of: nav.vm)
            }
        }
        .task(id: fileUrl) {
            graphRepository = .loading
            do {
                graphRepository = .loaded(try await FilesystemGraphRepository(basePath: fileUrl, filePresenterManager: filePresenterManager))
            } catch is CancellationError {
                graphRepository = .loading
            } catch {
                logError(error)
                graphRepository = .failed(error)
            }
        }
        .task {
            await filePresenterManager.unbackground()
            _ = try? await Task.sleepForever()
            await filePresenterManager.background()
        }
        .task {
            let originUrl = URL(fileURLWithPath: "/Users/devin/iCloud Drive/pictures/000000000000.json")

            class FilePresenter: NSObject, NSFilePresenter {
                let url: URL

                init(url: URL) {
                    self.url = url
                }

                // MARK: NSFilePresenter

                var presentedItemURL: URL? { url }
                var presentedItemOperationQueue: OperationQueue = .init()

                func relinquishPresentedItem(toWriter writer: @escaping @Sendable ((@Sendable () -> Void)?) -> Void) {
                    print("relinquishing ownership for writing")
                    writer {
                        print("regained ownership for writing")
                    }
                    do {
                        guard let contents = String(data: try Data(contentsOf: url), encoding: .utf8) else {
                            print("couldn't get contents")
                            return
                        }
                        print(contents)
                    } catch {
                        print("\(error)")
                    }
                }

                func presentedItemDidChange() {
                    print("presented item changed")
                    do {
                        guard let contents = String(data: try Data(contentsOf: url), encoding: .utf8) else {
                            print("couldn't get contents")
                            return
                        }
                        print(contents)
                    } catch {
                        print("\(error)")
                    }
                }

                func relinquishPresentedItem(toReader reader: @escaping @Sendable ((@Sendable () -> Void)?) -> Void) {
                    print("relinquishing ownership for reading")
                    reader {
                        print("regained ownership for reading")
                    }
                }
            }

            let filePresenter = FilePresenter(url: originUrl)
            NSFileCoordinator.addFilePresenter(filePresenter)
        }
        .navigationViewStyle(StackNavigationViewStyle())
    }
}
