//
//  NodeView.swift
//  view
//
//  Created by Devin Lehmacher on 10/15/22.
//

import SwiftUI
import AsyncButton

struct NodeView: View {
    @StateObject var vm: SemiSynchronousNodeVM<DefaultNode>

    init(of node: SemiSynchronousNodeVM<DefaultNode>) {
        _vm = StateObject(wrappedValue: node)
    }

    @State var showingTags: Bool = false
    @State var showingLinks: Bool = false
    @State var showingForceDeleteNodeConfirmation: Bool = false

    @ViewBuilder
    func links(_ state: SemiSynchronousNodeVM<DefaultNode>.State) -> some View {
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

    @ViewBuilder
    func loadedContent(_ state: SemiSynchronousNodeVM<DefaultNode>.State) -> some View {
        if let data = state.data, !showingLinks {
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
            links(state)
        }
    }

    @ViewBuilder
    var content: some View {
        switch vm.state {
        case .idle:
            Text("Idle")
        case .loading:
            ProgressView()
        case .failed(let error):
            ErrorIndicator(for: error)
        case .loaded(let state):
            loadedContent(state)
                .toolbar {
                    ToolbarItem(placement: .navigationBarTrailing) {
                        Menu {
                            Button(
                                "Force delete node",
                                role: .destructive) {
                                    showingForceDeleteNodeConfirmation = true
                                }
                            // TODO: "smart" actions that lists some common actions on nodes that can be done quickly
                            // e.g. apply specific tags
                        } label: {
                            Image(systemName: "list.bullet")
                        }
                    }
                    ToolbarItem(placement: .navigationBarTrailing) {
                        FillableSymbolToggle(isActive: state.tags.contains("to-update"), fillableSymbol: .bookmark) {
                            // TODO: cleanup sus potential race condition access
                            // one approach might be to have a value in the graph saying if an operation is in progress, this could be passed as an environment value and controls could use it to disable themselves
                            // of course this requires us to trust ourselves to remember to disable all relevant controls
                            // plus this makes controls somewhat less reusable, and wouldn't work if we threw a random Button in the mix somewhere
                            if state.tags.contains("to-update") {
                                await state.set(tags: state.tags.removing("to-update"))
                            } else {
                                await state.set(tags: state.tags.inserting("to-update"))
                            }
                        }
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
                    TagEditor(
                        initial: state.tags,
                        options: [String](state.possibleTags).sorted(),
                        commit: { newTags in
                            await state.set(tags: newTags)
                            showingTags = false
                        },
                        cancel: { showingTags = false })
                }
                .alert(
                    "Really force delete node?",
                    isPresented: $showingForceDeleteNodeConfirmation,
                    actions: {
                        AsyncButton("Delete", role: .destructive) {
                            // TODO: make state a state machine that transitions to a deleted state
                            await state.forceRemove()
                        }
                    },
                    message: {
                        Text("Force deleting a node is not reversible.")
                    })
        }
    }

    var body: some View {
        content
            .navigationTitle(Text("\(vm.nid)"))
            .task { await vm.load() }
    }
}

struct NodeView_Previews: PreviewProvider {
    static var previews: some View {
        Text("preview not supported")
    }
}
