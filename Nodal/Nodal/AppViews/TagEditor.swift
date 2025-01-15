//
//  TagEditor.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/2/25.
//

import SwiftUI

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
                Button(action: { commitAction(actual) }) {
                    Text("Commit")
                }
            }
            .buttonStyle(.bordered)
            .padding()
        }
    }
}

