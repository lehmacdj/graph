//
//  WebView.swift
//  view
//
//  Created by Devin Lehmacher on 1/3/22.
//

import SwiftUI
import SafariServices

struct SafariView: UIViewControllerRepresentable {
    let url: URL

    func makeUIViewController(context: Context) -> SFSafariViewController {
        return SFSafariViewController(url: url)
    }

    func updateUIViewController(_ safariView: SFSafariViewController, context: Context) {
    }
}
