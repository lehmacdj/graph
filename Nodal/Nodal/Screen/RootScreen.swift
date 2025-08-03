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

    @Environment(\.filePresenterManager) var filePresenterManager
    @State private var graphRepository: Loading<GraphRepository> = .loading
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
                        Image(systemName: "folder")
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
        .navigationViewStyle(StackNavigationViewStyle())
    }
}
