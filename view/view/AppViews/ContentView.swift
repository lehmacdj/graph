//
//  ContentView.swift
//  view
//
//  Created by Devin Lehmacher on 10/10/21.
//

import AsyncButton
import SwiftUI

struct ContentView: View {
    let fileUrl: URL
    let doSelectFile: () -> ()

    @State private var graphAndRoot: Loading<(GraphManager<DefaultNode>, DefaultNode)?> = .loading

    var body: some View {
        NavigationView {
            HStack {
                switch graphAndRoot {
                case .idle:
                    Text("Idle")
                case .loading:
                    Text("Loading...")
                case .loaded(nil):
                    Text("Couldn't read directory; invalid graph").foregroundColor(.red)
                case .loaded(.some((let graph, let origin))):
                    NodeView(of: NodeVM(for: origin.nid, in: graph))
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
        }
        .task(id: fileUrl) {
            graphAndRoot = .loading
            guard let graphManager = await GraphManager<DefaultNode>(dir: fileUrl) else {
                graphAndRoot = .loaded(nil)
                return
            }
            let origin = await graphManager.origin
            graphAndRoot = .loaded((graphManager, origin))
        }
        .navigationViewStyle(StackNavigationViewStyle())
    }
}

struct ImageView: View {
    let uiImage: UIImage
    @State var navigationVisible: Bool = false

    var body: some View {
        ZoomableScrollView {
            Image(uiImage: uiImage)
        }
        .overlay(navigationVisible ? ImageStats(uiImage: uiImage) : nil, alignment: .bottomTrailing)
        .onTapGesture { navigationVisible = !navigationVisible }
        .ignoresSafeArea(.all)
        .navigationBarHidden(!navigationVisible)
        .statusBar(hidden: !navigationVisible)
    }
}

struct ImageStats: View {
    let uiImage: UIImage

    @ViewBuilder
    var body: some View {
        Text("\(Int(uiImage.size.width)) x \(Int(uiImage.size.height))")
            .padding()
            .background(.background, in: Capsule())
            .opacity(0.7)
    }
}

struct TagEditor: View {
    init(initial: Set<String>, options: [String], commit: @escaping (Set<String>) async -> (), cancel: @escaping () async -> ()) {
        self.actual = initial
        self.options = options
        self.commitAction = commit
        self.cancelAction = cancel
    }

    @State var actual: Set<String>
    let options: [String]
    let commitAction: (Set<String>) async -> ()
    let cancelAction: () async -> ()

    @State var searchString: String = ""

    var body: some View {
        VStack {
            List(options, selection: $actual) { tag in
                Text(tag)
            }
            .environment(\.editMode, .constant(.active))
            .searchable(text: $searchString, placement: .automatic, prompt: "Tag search ...")
            HStack {
                AsyncButton(action: cancelAction) {
                    Text("Cancel")
                }
                AsyncButton(action: { await commitAction(actual)}) {
                    Text("Commit")
                }
            }
            .buttonStyle(.bordered)
            .padding()
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        Text("Preview not supported")
    }
}
