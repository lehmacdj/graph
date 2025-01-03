//
//  TagEditor.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/2/25.
//

import SwiftUI
import AsyncButton

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

