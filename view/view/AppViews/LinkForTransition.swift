//
//  LinkForTransition.swift
//  view
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

struct LinkForTransition: View {
    @ObservedObject var source: Node
    @ObservedObject var destination: Node
    let transition: String
    let direction: LinkDirection // only forward links get certain modification actions

    enum LinkDirection {
        case forward
        case backward
    }

    @State var confirmingDelete: Bool = false
    @State var editing: Bool = false

    init(from source: Node, to destination: Node, via transition: String, direction: LinkDirection) {
        self.source = source
        self.destination = destination
        self.transition = transition
        self.direction = direction
    }

    var body: some View {
        HStack {
            if let data = destination.data,
               let uiImage = UIImage(data: data) {
                Image(uiImage: uiImage)
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(maxHeight: 120)
            }
            if editing {
                LabelEditor(label: transition, displayed: $editing) { newLabel in
                    source.root.removeLink(from: source, to: destination, via: transition)
                    source.root.addLink(from: source, to: destination, via: newLabel)
                }
            } else {
                NavigationLink(destination: NodeView(of: destination)) {
                    Text(transition)
                }
            }
        }
        .alert("Really delete transition?", isPresented: $confirmingDelete) {
            Button("Delete", role: .destructive) {
                source.root.removeLink(from: source, to: destination, via: transition)
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
            if direction == .forward {
                Button() {
                    source.toggleFavorite(child: destination)
                } label: {
                    if source.isFavorite(child: destination.nid) {
                        Label("Unfavorite", systemImage: "star.fill")
                    } else {
                        Label("Favorite", systemImage: "star")
                    }
                }

                .tint(.yellow)
                Button() {
                    source.toggleWorse(child: destination)
                } label: {
                    if source.isWorse(child: destination.nid) {
                        Label("Unworsen", systemImage: "xmark.bin.fill")
                    } else {
                        Label("Worsen", systemImage: "xmark.bin")
                    }
                }
                .tint(.purple)
            }
        }
    }
}

struct LinkForTransition_Previews: PreviewProvider {
    static var previews: some View {
        Text("preview not supported for LinkForTransition")
    }
}
