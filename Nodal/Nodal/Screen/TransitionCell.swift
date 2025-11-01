//
//  LinkForTransition.swift
//  Nodal
//
//  Created by Devin Lehmacher on 10/19/22.
//

import SwiftUI

struct LabelEditor: View {
    @State var label: String
    @Binding var displayed: Bool
    let action: (String) -> ()

    var body: some View {
        HStack {
            TextField("Label", text: $label)
            Button("Cancel", role: .cancel) {
                displayed = false
            }
            // without applying this button style the button takes over the entire row via List's behavior
            .buttonStyle(BorderlessButtonStyle())
            Button("Confirm") {
                action(label)
                displayed = false
            }
            // without applying this button style the button takes over the entire row via List's behavior
            .buttonStyle(BorderlessButtonStyle())
        }
    }
}

@MainActor
struct TransitionCell: View {
    @State var vm: AnyTransitionVM
    @State private var confirmingDelete: Bool = false

    init(_ transitionVM: AnyTransitionVM) {
        _vm = State(wrappedValue: transitionVM)
    }

    @ViewBuilder
    var thumbnail: some View {
        Suspense(vm.thumbnail) { loadingThumbnail in
            switch loadingThumbnail {
            case .noThumbnail:
                EmptyView()
            case .cloudFile:
                Image(systemName: "cloud")
            case .thumbnail(let loadingThumbnail):
                Suspense(loadingThumbnail) { thumbnail in
                    Image(uiImage: thumbnail)
                        .resizable()
                        .aspectRatio(contentMode: .fit)
                }
                .frame(height: 120)
            }
        }
    }

    @ViewBuilder
    var cellButton: some View {
        if case .loaded(.cloudFile) = vm.thumbnail {
            Button(role: .none) {
                vm.fetchThumbnail()
            } label: {
                content
            }
        } else {
            NavigationLink(value: vm.destination.nav) {
                content
            }
        }
    }

    @ViewBuilder
    var tags: some View {
        Suspense(vm.tags) { tags in
            ForEach(tags) { tag in
                Text("#\(tag)")
                    .pillStyled()
            }
        }
    }

    @ViewBuilder
    var timestamp: some View {
        Suspense(vm.timestamp) { timestamp in
            if let timestamp {
                TimestampView(timestamp: timestamp)
                    .pillStyled()
            }
        } placeholder: {
            // slightly hacky: avoid showing two spinners if both are showing
            if vm.tags.isLoaded {
                ProgressView()
            }
        }
    }

    @ViewBuilder
    var content: some View {
        HStack {
            thumbnail
            ViewThatFits {
                HStack(spacing: 4) {
                    Text(vm.transition)
                    Spacer()
                    // it's useful for tags to still be in a flow layout
                    // because their might be a thumbnail that gives more than
                    // 1 line of vertical space
                    FlowLayout(spacing: 4) {
                        tags
                    }
                    // if rendering tags/timestamp right aligned we want the
                    // timestamp to come last
                    timestamp
                }
                VStack(alignment: .leading, spacing: 4) {
                    Text(vm.transition)
                    FlowLayout(spacing: 4) {
                        // timestamp comes first to be more scannable
                        timestamp
                        tags
                    }
                }
            }
            
        }
    }

    var body: some View {
        cellButton
            .alert("Really delete transition?", isPresented: $confirmingDelete) {
                Button("Delete", role: .destructive) {
                    vm.removeTransition()
                }
            } message: {
                Text("Deleting a transition is not reversible.")
            }
            .swipeActions(edge: .trailing) {
                // this button can't be role: .destructive because if it
                // is SwiftUI tries to be smart by removing the list item
                // but that cancels the confirmation dialogue that is attached
                // to the list item
                Button(role: .destructive) {
                    confirmingDelete = true
                } label: {
                    Label("Delete", systemImage: "trash")
                }

                Button() {} label: {
                    Label("More", systemImage: "pencil")
                }
                .tint(.orange)
            }
            .swipeActions(edge: .leading) {
                if vm.direction == .forward {
                    Button() {
                        vm.toggleFavorite()
                    } label: {
                        if vm.isFavorite {
                            Label("Unfavorite", systemImage: "star.fill")
                        } else {
                            Label("Favorite", systemImage: "star")
                        }
                    }
                    .tint(.yellow)

                    Button {
                        vm.toggleWorse()
                    } label: {
                        if vm.isWorse {
                            Label("Unworsen", systemImage: "xmark.bin.fill")
                        } else {
                            Label("Worsen", systemImage: "xmark.bin")
                        }
                    }
                    .tint(.purple)
                }
            }
            .task { await vm.subscribe() }
    }
}

private extension View {
    func pillStyled() -> some View {
        self
            .font(.caption2)
            .padding(4)
            .foregroundColor(.secondary)
            .overlay(
                RoundedRectangle(cornerRadius: 4)
                    .stroke(Color.secondary, lineWidth: 1)
            )
    }
}

#if DEBUG
#Preview {
    NavigationStack {
        List {
            TransitionCell(
                MockTransitionVM(
                    transition: "Million Live",
                    thumbnail: .loaded(.thumbnail(.loaded(.millionLive11))),
                    tags: .loaded(["millions", "of", "tags", "that", "cause", "horizontal", "space", "to", "overflow"])
                ).eraseToAnyTransitionVM()
            )
            TransitionCell(
                MockTransitionVM(
                    transition: "More More Jump",
                    thumbnail: .loaded(.thumbnail(.loaded(.moreMoreJumpBackground))),
                    timestamp: .loaded(Date(timeIntervalSince1970: 0))
                ).eraseToAnyTransitionVM()
            )
            TransitionCell(MockTransitionVM(transition: "foo").eraseToAnyTransitionVM())
            TransitionCell(
                MockTransitionVM(
                    transition: "bar",
                    tags: .loaded(["millions", "of", "tags", "that", "cause", "horizontal", "space", "to", "overflow"])
                ).eraseToAnyTransitionVM()
            )
            TransitionCell(
                MockTransitionVM(
                    transition: "Hello world!",
                    timestamp: .loaded(Date(timeIntervalSince1970: 0)),
                    tags: .loaded(["hello", "world"])
                ).eraseToAnyTransitionVM()
            )
            TransitionCell(
                MockTransitionVM(
                    transition: "Only loading tags",
                    timestamp: .loaded(Date(timeIntervalSince1970: 0)),
                    tags: .loading
                ).eraseToAnyTransitionVM()
            )
            TransitionCell(
                MockTransitionVM(
                    transition: "Loading tags and timestamp",
                    timestamp: .loading,
                    tags: .loading
                ).eraseToAnyTransitionVM()
            )
            TransitionCell(
                MockTransitionVM(
                    transition: "Only loading timestamp",
                    timestamp: .loading,
                    tags: .loaded(["hello", "world"])
                ).eraseToAnyTransitionVM()
            )
        }
    }
}
#endif
