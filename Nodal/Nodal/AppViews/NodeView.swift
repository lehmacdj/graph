//
//  NodeView.swift
//  Nodal
//
//  Created by Devin Lehmacher on 10/15/22.
//

import SwiftUI
import UniformTypeIdentifiers

@MainActor
struct NodeView: View {
    @State var vm: AnyNodeVM

    init(of node: AnyNodeVM) {
        _vm = State(wrappedValue: node)
    }

    @State var showingLinks: Bool = false
    @State var showingForceDeleteNodeConfirmation: Bool = false

    @ViewBuilder
    func links(_ state: NodeState) -> some View {
        List {
            if let favorites = state.favoriteLinks {
                Section("Favorites") {
                    ForEach(favorites) { favorite in
                        TransitionView(favorite)
                    }
                }
            }

            Section {
                ForEach(state.links) { link in
                    TransitionView(link)
                }
            }

            Section("Backlinks") {
                ForEach(state.backlinks) { backlink in
                    TransitionView(backlink)
                }
            }

            if let worses = state.worseLinks {
                Section("Worse") {
                    ForEach(worses) { worse in
                        TransitionView(worse)
                    }
                }
            }
        }
    }

    @State var navigationVisible = true

    @ViewBuilder
    func loadedContent(_ state: NodeState) -> some View {
        if let data = state.data, !showingLinks {
            if let uiImage = UIImage(data: data) {
                ImageView(uiImage: uiImage, extraContentVisible: navigationVisible)
                    .onTapGesture { navigationVisible = !navigationVisible }
                    .navigationBarHidden(!navigationVisible)
                    .statusBar(hidden: !navigationVisible)
            } else if let string = String(data: data, encoding: .utf8),
                      let url = URL(string: string.trimmingCharacters(in: .whitespacesAndNewlines)),
                      url.scheme?.starts(with: "http") ?? false {
                SafariView(url: url)
            } else {
                // TODO: add support for more data types
                Text("Node has data with unknown kind")
            }
        } else {
            links(state)
                .refreshable { await vm.reload() }
        }
    }

    @ToolbarContentBuilder
    func toolbarContent(for state: NodeState) -> some ToolbarContent {
        ToolbarItem(placement: .navigationBarTrailing) {
            Button {
                UIPasteboard.general.string = String(describing: vm.nid)
            } label: {
                Image(systemName: "link")
            }
        }
        ToolbarItem(placement: .navigationBarTrailing) {
            Menu {
                Button("Force delete node", role: .destructive) {
                    showingForceDeleteNodeConfirmation = true
                }
                // TODO: "smart" actions that lists some common actions on nodes that can be done quickly
                // e.g. apply specific tags
            } label: {
                Image(systemName: "ellipsis")
            }
        }
        if state.data != nil {
            ToolbarItem(placement: .navigationBarTrailing) {
                Button(action: { showingLinks.toggle() }) {
                    if showingLinks {
                        Image(systemName: "eye.fill")
                    } else {
                        Image(systemName: "list.bullet")
                    }
                }
            }
        }
    }

    @ViewBuilder
    func content(for state: NodeState) -> some View {
        loadedContent(state)
            .toolbar { toolbarContent(for: state) }
            .alert(
                "Really force delete node?",
                isPresented: $showingForceDeleteNodeConfirmation,
                actions: {
                    Button("Delete", role: .destructive) {
                        // TODO: make state a state machine that transitions to a deleted state
                        do {
                            try vm.forceRemove()
                        } catch {
                            logError("error while trying to force remove node: \(error)")
                        }
                    }
                },
                message: {
                    Text("Force deleting a node is not reversible.")
                }
            )
    }

    var body: some View {
        Suspense(vm.state) { state in
            content(for: state)
        }
        .navigationTitle(Text("\(vm.nid)"))
        .task { await vm.subscribe() }
    }
}

#if DEBUG
#Preview {
    NavigationStack {
        NodeView(
            of: MockNodeVM(
                nid: NID(fake: 91),
                data: nil,
                favoriteLinks: [
                    .init(transition: "some favorite")
                ],
                links: [
                    .init(thumbnail: .loaded(.thumbnail(.loaded(.millionLive11)))),
                    .init(thumbnail: .loaded(.thumbnail(.loaded(.moreMoreJumpBackground)))),
                    .init(transition: "some named link"),
                    .init(transition: "some other link"),
                ],
                tags: ["foo", "bar", "baz", "white", "red", "green", "blue", "purple"]
            )
            .eraseToAnyNodeVM()
        )
    }
}
#endif
