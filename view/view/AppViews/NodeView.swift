//
//  NodeView.swift
//  view
//
//  Created by Devin Lehmacher on 10/15/22.
//

import SwiftUI

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

struct NodeView: View {
    @StateObject var node: Node

    init(of node: Node) {
        _node = StateObject(wrappedValue: node)
    }

    @State var showingTags: Bool = false
    @State var showingLinks: Bool = false
    @State var showingForceDeleteNodeConfirmation: Bool = false

    @ViewBuilder
    func linkForTransition(_ item: NodeTransition) -> some View {
        if let destination = self.node.root[item.nid] {
            LinkForTransition(from: node, to: destination, via: item.transition)
        } else {
            Text("Transition \(item.transition) to nonexistent node \(item.nid)")
        }
    }

    @ViewBuilder
    var content: some View {
        if let data = node.data, !showingLinks {
            if let uiImage = UIImage(data: data) {
                ImageView(uiImage: uiImage)
            } else if let string = String(data: data, encoding: .utf8),
                      let url = URL(string: string.trimmingCharacters(in: .whitespacesAndNewlines)),
                      url.scheme?.starts(with: "http") ?? false {
                SafariView(url: url)
            } else {
                // TODO: add support for more data types
                Text("Node has data with unknown kind")
            }
        } else {
            List {
                Section {
                    ForEach(node.outgoing.sorted(on: { [node.isFavorite(child: $0.nid) ? "" : " ", $0.transition] } )) { item in
                        linkForTransition(item)
                    }
                }
                Section("Backlinks") {
                    ForEach(node.incoming.sorted(on: \.transition)) { item in
                        linkForTransition(item)
                    }
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
                        .contextMenu {
                            // TODO also want menu item for normal action
                            Button(
                                "Force delete node",
                                role: .destructive) {
                                showingForceDeleteNodeConfirmation = true
                            }
                        }
                }
                ToolbarItem(placement: .navigationBarTrailing) {
                    TagToggler(node: node, tag: "to-update", fillableSymbol: "bookmark")
                }
                ToolbarItem(placement: .navigationBarTrailing) {
                    TagToggler(node: node, tag: "to-favorite", fillableSymbol: "star")
                }
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(action: { showingTags.toggle() }) {
                        Image(systemName: "tag")
                    }
                }
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(action: { showingLinks.toggle() }) {
                        Image(systemName: "link")
                    }
                }
            }
            .sheet(isPresented: $showingTags) {
                if let tags = node.tags,
                   let tagOptions = node.root.tags?.tagOptions {
                    TagEditor(
                        initial: tags,
                        options: [String](tagOptions).sorted(),
                        commit: { newTags in
                            node.tags = newTags
                            showingTags = false
                        },
                        cancel: { showingTags = false })
                } else {
                    Text("Error couldn't find tags to present")
                }
            }
            .alert(
                "Really force delete node?",
                isPresented: $showingForceDeleteNodeConfirmation,
                actions: {
                    Button("Delete", role: .destructive) {
                        node.root.forceRemove(node: node)
                    }
                },
                message: {
                    Text("Force deleting a node is not reversible.")
                })
    }
}

struct NodeView_Previews: PreviewProvider {
    static var previews: some View {
        Text("preview not supported")
    }
}
