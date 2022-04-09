//
//  ContentView.swift
//  view
//
//  Created by Devin Lehmacher on 10/10/21.
//

import SwiftUI
import QuickLook
import QuickLookThumbnailing

struct ContentView: View {
    let fileUrl: URL
    let doSelectFile: () -> ()

    var body: some View {
        NavigationView {
            NodeView(of: Graph(dir: fileUrl).origin)
                .toolbar {
                    ToolbarItem(placement: .navigationBarLeading) {
                        Button(action: doSelectFile) {
                            Text("Select graph")
                        }
                    }
                }
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
        .onTapGesture { navigationVisible = !navigationVisible }
        .ignoresSafeArea(.all)
        .navigationBarHidden(!navigationVisible)
        .statusBar(hidden: !navigationVisible)
    }
}

struct NodePreviewView: View {
    let label: String
    @ObservedObject var node: Node

    var body: some View {
        if let data = node.data,
           let uiImage = UIImage(data: data) {
            HStack {
                Image(uiImage: uiImage)
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(maxHeight: 120)
                Text(label)
            }
        } else {
            Text(label)
        }
    }
}

struct NodeView: View {
    @StateObject var node: Node

    init(of node: Node) {
        _node = StateObject(wrappedValue: node)
    }

    @State var showingTags: Bool = false

    var itemsToDisplay: [ListItem] {
        var toDisplay = [ListItem]()
        if let data = node.data {
            toDisplay.append(.nodeDataPreview(data))
        }
        toDisplay.append(
            contentsOf: node.meta.outgoing
                .sorted(on: \.key)
                .flatMap { kv in
                    kv.value.map { .transitionTo(kv.key, $0) }
                })
        return toDisplay
    }

    @ViewBuilder
    var content: some View {
        if node.outgoing.isEmpty,
           let data = node.data,
           let uiImage = UIImage(data: data) {
            ImageView(uiImage: uiImage)
        } else if node.outgoing.isEmpty,
                  let data = node.data,
                  let string = String(data: data, encoding: .utf8),
                  let url = URL(string: string.trimmingCharacters(in: .whitespacesAndNewlines)),
                  url.scheme?.starts(with: "http") ?? false {
            SafariView(url: url)
        } else {
            List(itemsToDisplay) { item in
                listItemView(for: item)
            }
        }
    }

    struct TagToggler: View {
        @ObservedObject var node: Node
        let tag: String
        let active: String
        let inactive: String

        init(node: Node, tag: String, fillableSymbol: String) {
            self.node = node
            self.tag = tag
            self.active = "\(fillableSymbol).fill"
            self.inactive = fillableSymbol
        }

        private func toggleTag() {
            var tags = node.tags
            if tags.contains(tag) {
                tags.remove(tag)
                node.tags = tags
            } else {
                tags.insert(tag)
                node.tags = tags
            }
        }

        var body: some View {
            Button(action: toggleTag) {
                if node.tags.contains(tag) {
                    Image(systemName: active)
                } else {
                    Image(systemName: inactive)
                }
            }
        }
    }

    var body: some View {
        content
            .navigationTitle(Text("\(node.meta.id)"))
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    TagToggler(node: node, tag: "to-worsen", fillableSymbol: "trash")
                }
                ToolbarItem(placement: .navigationBarTrailing) {
                    TagToggler(node: node, tag: "to-update", fillableSymbol: "bookmark")
                }
                ToolbarItem(placement: .navigationBarTrailing) {
                    TagToggler(node: node, tag: "to-favorite", fillableSymbol: "star")
                }
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(action: { showingTags.toggle() }) {
                        Text("Tags")
                    }
                }
            }
            .sheet(isPresented: $showingTags) {
                if let tags = node.tags,
                   let tagOptions = node.root.tags?.tagOptions {
                    TagEditor(
                        initial: tags,
                        options: [String](tagOptions),
                        commit: { newTags in
                            node.tags = newTags
                            showingTags = false
                        },
                        cancel: { showingTags = false })
                } else {
                    Text("Error couldn't find tags to present")
                }
            }
    }

    enum ListItem: Identifiable {
        case nodeDataPreview(_ data: Data)
        case transitionTo(_ label: String, _ nid: NID)

        var id: String {
            switch self {
            case .nodeDataPreview(_):
                return "nodeDataPreview"
            case .transitionTo(let l, let nid):
                return "\(l)->\(nid)"
            }
        }
    }

    @ViewBuilder
    func listItemView(for item: ListItem) -> some View {
        switch item {
        case .nodeDataPreview(let data):
            if let uiImage = UIImage(data: data) {
                NavigationLink(destination: ImageView(uiImage: uiImage)) {
                    NodePreviewView(label: "This node has data!", node: node)
                }
            } else if let str = String(data: data, encoding: .utf8),
                      let url = URL(string: str.trimmingCharacters(in: .whitespacesAndNewlines)),
                      url.scheme?.starts(with: "http") ?? false {
                NavigationLink(destination: SafariView(url: url)) {
                    NodePreviewView(label: "This node has a url!", node: node)
                }
            } else {
                // TODO: support more data kinds
                Text("Unknown data kind")
            }
        case .transitionTo(let l, let nid):
            if let node = self.node.root[nid] {
                NavigationLink(destination: NodeView(of: node)) {
                    NodePreviewView(label: l, node: node)
                }
            } else {
                Text("Transition \(l) to nonexistent node \(nid)")
            }
        }
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
        Text("Preview not supported yet!")
    }
}
