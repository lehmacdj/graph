//
//  ImageView.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/2/25.
//

import SwiftUI

struct ImageView: View {
    let uiImage: UIImage
    let extraContentVisible: Bool

    var body: some View {
        ZoomableView {
            Image(uiImage: uiImage)
        }
        .overlay(extraContentVisible ? ImageStats(uiImage: uiImage) : nil, alignment: .bottomTrailing)
        .ignoresSafeArea(.all)
        .preventSleeping(!extraContentVisible)
    }
}

private struct ImageStats: View {
    let uiImage: UIImage

    @ViewBuilder
    var body: some View {
        Text("\(Int(uiImage.size.width)) x \(Int(uiImage.size.height))")
            .padding()
            .background(.background, in: Capsule())
            .opacity(0.7)
    }
}

