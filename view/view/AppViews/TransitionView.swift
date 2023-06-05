//
//  LinkForTransition.swift
//  view
//
//  Created by Devin Lehmacher on 10/19/22.
//

import SwiftUI
import AsyncButton

struct LabelEditor: View {
    @State var label: String
    @Binding var displayed: Bool
    let action: (String) async -> ()

    var body: some View {
        HStack {
            TextField("Label", text: $label)
            Button("Cancel", role: .cancel) {
                displayed = false
            }
            // without applying this button style the button takes over the entire row via List's behavior
            .buttonStyle(BorderlessButtonStyle())
            AsyncButton("Confirm") {
                await action(label)
                displayed = false
            }
            // without applying this button style the button takes over the entire row via List's behavior
            .buttonStyle(BorderlessButtonStyle())
        }
    }
}

struct TransitionView: View {
    @StateObject var vm: TransitionVM

    @State private var confirmingDelete: Bool = false
    @State private var editing: Bool = false

    init(_ transitionVM: TransitionVM) {
        _vm = StateObject(wrappedValue: transitionVM)
    }

    var thumbnail: some View {
        Suspense(vm.thumbnail) { loadingThumbnail in
            if let loadingThumbnail {
                Suspense(loadingThumbnail) { thumbnail in
                    Image(uiImage: thumbnail)
                        .resizable()
                        .aspectRatio(contentMode: .fit)
                }
                .frame(height: 120)
            }
        } placeholder: {
            // until we know if we have a thumbnail assume we don't
            EmptyView()
        }
    }

    var body: some View {
        HStack {
            thumbnail

            if editing {
                LabelEditor(label: vm.transition, displayed: $editing) { newLabel in
                    await vm.updateTransitionName(to: newLabel)
                }
            } else {
                Suspense(vm.destination) { destination in
                    NavigationLink(destination: NodeView(of: destination)) {
                        Text(vm.transition)
                    }
                }
            }
        }
        .alert("Really delete transition?", isPresented: $confirmingDelete) {
            AsyncButton("Delete", role: .destructive) {
                await vm.removeTransition()
            }
        } message: {
            Text("Deleting a transition is not reversible.")
        }
        .swipeActions(edge: .trailing) {
            // this button can't be role: .destructive because if it
            // is SwiftUI tries to be smart by removing the list item
            // but that cancels the confirmation dialogue that is attached
            // to the list item
            Button() {
                confirmingDelete = true
            } label: {
                Label("Delete", systemImage: "trash")
            }
            .tint(.red)

            Button() {
                editing = true
            } label: {
                Label("More", systemImage: "pencil")
            }
            .tint(.orange)
        }
        .swipeActions(edge: .leading) {
            if vm.direction == .forward {
                AsyncButton() {
                    await vm.toggleFavorite()
                } label: {
                    if vm.isFavorite {
                        Label("Unfavorite", systemImage: "star.fill")
                    } else {
                        Label("Favorite", systemImage: "star")
                    }
                }
                .tint(.yellow)

                AsyncButton() {
                    await vm.toggleWorse()
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
        .task { await vm.load() }
        .onDisappear { vm.weaken() }
    }
}

struct LinkForTransition_Previews: PreviewProvider {
    static var previews: some View {
        Text("preview not supported for LinkForTransition")
    }
}
