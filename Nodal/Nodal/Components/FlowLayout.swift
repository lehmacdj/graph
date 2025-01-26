//
//  FlowLayout.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/26/25.
//

import SwiftUI

public struct FlowLayout: Layout {
    public let horizontalSpacing: CGFloat
    public let verticalSpacing: CGFloat

    public init(horizontalSpacing: CGFloat = 8, verticalSpacing: CGFloat = 8) {
        self.horizontalSpacing = horizontalSpacing
        self.verticalSpacing = verticalSpacing
    }

    public init(spacing: CGFloat) {
        self.init(horizontalSpacing: spacing, verticalSpacing: spacing)
    }

    /// Compute positions and the overall size of the FlowLayout by using the sizes of the subviews and a specific width
    /// This just naively goes row by row trying to place as many subviews as possible in a row, then moving to the next
    /// row.
    private func layout(
        sizes: [CGSize],
        containerWidth: CGFloat
    ) -> (offsets: [CGPoint], size: CGSize) {
        var result: [CGPoint] = []
        var currentPosition: CGPoint = .zero
        var lineHeight: CGFloat = 0
        var maxX: CGFloat = 0

        for size in sizes {
            if currentPosition.x + size.width > containerWidth {
                currentPosition.x = 0
                currentPosition.y += lineHeight + verticalSpacing
                lineHeight = 0
            }

            result.append(currentPosition)
            currentPosition.x += size.width
            maxX = max(maxX, currentPosition.x)
            currentPosition.x += horizontalSpacing
            lineHeight = max(lineHeight, size.height)
        }

        return (result, CGSize(width: maxX, height: currentPosition.y + lineHeight))
    }

    public func sizeThatFits(
        proposal: ProposedViewSize,
        subviews: Subviews,
        cache _: inout ()
    ) -> CGSize {
        let containerWidth = proposal.width ?? .infinity
        let sizes = subviews.map { $0.sizeThatFits(.unspecified) }

        return layout(
            sizes: sizes,
            containerWidth: containerWidth
        ).size
    }

    public func placeSubviews(
        in bounds: CGRect,
        proposal _: ProposedViewSize,
        subviews: Subviews,
        cache _: inout ()
    ) {
        let sizes = subviews.map { $0.sizeThatFits(.unspecified) }
        let offsets = layout(
            sizes: sizes,
            containerWidth: bounds.width
        ).offsets

        for (offset, subview) in zip(offsets, subviews) {
            subview.place(
                at: CGPoint(
                    x: offset.x + bounds.minX,
                    y: offset.y + bounds.minY
                ),
                proposal: .unspecified
            )
        }
    }
}

#if DEBUG
let colors: [Color] = [
    .blue,
    .yellow,
    .purple,
    .red,
    .orange,
]

let words = [
    "Hello",
    "world",
    "foo",
    "bar",
    "baz",
    "some",
    "other",
    "words",
    "because",
    "I",
    "ran",
    "out",
    "of",
    "my",
    "default",
    "filler",
]

#Preview {
    FlowLayout {
        ForEach(Array(words.enumerated()), id: \.0) { id, word in
            Text(word)
                .foregroundStyle(.white)
                .background(colors[id % colors.count])
        }
    }
}
#endif
