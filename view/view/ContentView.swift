//
//  ContentView.swift
//  view
//
//  Created by Devin Lehmacher on 10/10/21.
//

import SwiftUI

struct ContentView: View {
    @Binding var document: GraphDocument

    var body: some View {
        NodeView(of: document.root.origin)
    }
}

struct NodeView: View {
    let node: Node

    init(of node: Node) {
        self.node = node
    }
    
    var body: some View {
        List(node.transitions) { transition in
            NavigationLink(destination: NodeView(of: node[transition]!)) {
                Text(transition)
            }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        Text("Preview not supported yet!")
    }
}
