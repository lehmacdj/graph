//
//  ContentView.swift
//  view
//
//  Created by Devin Lehmacher on 10/10/21.
//

import SwiftUI

private enum Loading<T> {
    case loading
    case loaded(T)
}

struct ContentView: View {
    let fileUrl: URL
    let doSelectFile: () -> ()

    @State private var graph: Loading<Graph?> = .loading

    var body: some View {
        NavigationView {
            HStack {
                switch graph {
                case .loading:
                    Text("Loading...")
                case .loaded(nil):
                    Text("Couldn't read directory; invalid graph").foregroundColor(.red)
                case .loaded(let .some(graph)):
                    NodeView(of: graph.origin)
                        .refreshable {
                            await graph.refresh()
                        }
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
            graph = .loading
            graph = .loaded(await Graph(dir: fileUrl))
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
    init(initial: Set<String>, options: [String], commit: @escaping (Set<String>) -> (), cancel: @escaping () -> ()) {
        self.actual = initial
        self.options = options
        self.commitAction = commit
        self.cancelAction = cancel
    }

    @State var actual: Set<String>
    let options: [String]
    let commitAction: (Set<String>) -> ()
    let cancelAction: () -> ()

    @State var searchString: String = ""

    var body: some View {
        VStack {
            List(options, selection: $actual) { tag in
                Text(tag)
            }
            .environment(\.editMode, .constant(.active))
            .searchable(text: $searchString, placement: .automatic, prompt: "Tag search ...")
            HStack {
                Button(action: cancelAction) {
                    Text("Cancel")
                }
                Button(action: { commitAction(actual)}) {
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
