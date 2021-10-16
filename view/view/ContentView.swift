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

    var body: some View {
        NavigationView {
            NodeView(of: Root(dir: fileUrl).origin)
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
    let node: Node
    
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
    let node: Node
    
    init(of node: Node) {
        self.node = node
    }
    
    var itemsToDisplay: [ListItem] {
        var toDisplay = [ListItem]()
        if let data = node.data {
            toDisplay.append(.nodeDataPreview(data))
        }
        toDisplay.append(contentsOf: node.outgoing.sorted().map({.transition($0)}))
        return toDisplay
    }
    
    var body: some View {
        if node.outgoing.isEmpty,
           let data = node.data,
           let uiImage = UIImage(data: data) {
            ImageView(uiImage: uiImage)
        } else {
            // Consider using something like this to allow a grid to be used on iPad.
            // However, to get this looking good, it will take somewhat substantially
            // more tweaking to make it look similar to the original list view
            // ScrollView {
            //    LazyVGrid(columns: [GridItem(.adaptive(minimum: 300, maximum: 400))]) {
            //        ForEach(itemsToDisplay) { item in
            //            ListItemView(node: node, item: item)
            //        }
            //    }
            // }
            List(itemsToDisplay) { item in
                ListItemView(node: node, item: item)
            }
            .navigationTitle(Text("\(node.meta.id)"))
        }
    }
    
    enum ListItem: Identifiable {
        case nodeDataPreview(_ data: Data)
        case transition(_ label: String)
        
        var id: String {
            switch self {
            case .nodeDataPreview(_):
                return "nodeDataPreview"
            case .transition(let l):
                return "transition=\(l)"
            }
        }
    }
    
    struct ListItemView: View {
        let node: Node
        let item: ListItem
        
        var body: some View {
            switch item {
            case .nodeDataPreview(let data):
                if let uiImage = UIImage(data: data) {
                    NavigationLink(destination: ImageView(uiImage: uiImage)) {
                        NodePreviewView(label: "This node has data!", node: node)
                    }
                } else {
                    // TODO: support more data kinds
                    Text("Unknown data kind")
                }
            case .transition(let l):
                if let referencedNode = node[l] {
                    NavigationLink(destination: NodeView(of: referencedNode)) {
                        NodePreviewView(label: l, node: referencedNode)
                    }
                } else {
                    Text(
                        "Error, couldn't resolve node \(node.meta.outgoing[l].map(\.description) ?? "???")"
                        + " while trying to resolve transition \(l)"
                    )
                }
            }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        Text("Preview not supported yet!")
    }
}
