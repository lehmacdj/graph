//
//  ZoomedRotatedView.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/25.
//

import SwiftUI

@Animatable
struct ZoomedRotatedView<Content: View>: View {
    let content: Content

    var scale: CGFloat
    var rotation: Angle

    @AnimatableIgnored
    @State var contentSize: CGSize = .zero

    /// Size accounting for scale/rotation
    var transformedSize: CGSize {
        contentSize
            .applying(
                aboutCenter: CGAffineTransform(
                    scale: scale,
                    rotation: rotation,
                    anchor: .zero
                )
            )
            .absoluteValue
    }

    var body: some View {
        Color.clear.frame(size: transformedSize)
        .background {
            content
                .fixedSize()
                .onGeometryChange(for: CGSize.self) { proxy in
                    proxy.size
                } action: { size in
                    contentSize = size
                }
                .scaleEffect(CGSize(square: scale))
                .rotationEffect(rotation)
        }
    }
}

/// Shows the zoomed rotated view superimposed over the original
private struct ZoomedRotatedViewPreviewer<Content: View>: View {
    let content: Content
    let scale: CGFloat
    let rotation: Angle

    var body: some View {
        content.fixedSize()
            .overlay {
                ZoomedRotatedView(
                    content: content,
                    scale: scale,
                    rotation: rotation
                )
                .opacity(0.5)
                .border(Color.pink)
            }
    }
}

private struct MockImage: View {
    var body: some View {
        Rectangle()
            .foregroundStyle(
                Gradient(colors: [
                    .pink,
                    .purple,
                    .blue,
                    .green
                ])
            )
        .frame(width: 200, height: 300)
    }
}

#Preview {
    ZoomedRotatedViewPreviewer(
        content: MockImage(),
        scale: 1.0,
        rotation: .zero
    )
}

#Preview {
    ZoomedRotatedViewPreviewer(
        content: MockImage(),
        scale: 2.0,
        rotation: .zero
    )
}

#Preview {
    @Previewable @State var degrees: Double = 45
    ZoomedRotatedViewPreviewer(
        content: MockImage(),
        scale: 1.0,
        rotation: .degrees(degrees)
    )
    .onTapGesture {
        withAnimation(.linear(duration: 5)) {
            degrees = degrees + 360
        } completion: {
            degrees = 45
        }
    }
}
