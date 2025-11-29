//
//  NodeContextMenu.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/1/25.
//

import SwiftUI

@ViewBuilder
func nodeMenuItems(nid: NID, dataURL: URL?, transition: String?) -> some View {
    Button {
        UIPasteboard.general.string = String(describing: nid)
    } label: {
        Label(String(describing: nid), systemImage: "link")
    }
    if let dataURL {
        Button {
            UIPasteboard.general.url = dataURL
            UIPasteboard.general.string = dataURL.absoluteString
        } label: {
            Label("Copy data file path", systemImage: "document")
        }
    }
    if let transition {
        if let url = URL(string: transition),
           url.scheme != nil {
            Button {
                UIApplication.shared.open(url)
            } label: {
                Label("Open in browser", systemImage: "safari")
            }
        }
        Button {
            UIPasteboard.general.string = transition
        } label: {
            Label("Copy transition", systemImage: "character")
        }
    }
}
