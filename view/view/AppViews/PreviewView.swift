//
//  PreviewView.swift
//  view
//
//  Created by Devin Lehmacher on 10/23/21.
//

import QuickLook
import SwiftUI

struct PreviewView: UIViewControllerRepresentable {
    let urls: [URL]

    func makeUIViewController(context: Context) -> QLPreviewController {
        let controller = QLPreviewController()
        controller.dataSource = context.coordinator
        return controller
    }

    func makeCoordinator() -> Coordinator {
        return Coordinator(parent: self)
    }

    func updateUIViewController(
        _ uiViewController: QLPreviewController, context: Context) {}
}

class Coordinator: QLPreviewControllerDataSource {
    let parent: PreviewView

    init(parent: PreviewView) {
        self.parent = parent
    }

    func numberOfPreviewItems(in controller: QLPreviewController) -> Int {
        return parent.urls.count
    }

    func previewController(_ controller: QLPreviewController, previewItemAt index: Int) -> QLPreviewItem {
        return parent.urls[index] as NSURL
    }
}
