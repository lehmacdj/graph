//
//  ZoomableView.swift
//  Nodal
//
//  Created by Devin Lehmacher on 7/14/23.
//

import SwiftUI

enum ZoomState: Sendable {
    case fit
    case fill
    case actualSize

    var next: ZoomState {
        switch self {
        case .fit:
            return .fill
        case .fill:
            return .actualSize
        case .actualSize:
            return .fit
        }
    }
}

struct ZoomableView<Content: View>: View {
    private let content: Content

    init(content: () -> Content) {
        self.content = content()
    }

    @State private var scale: CGFloat = 1.0
    @State private var angle: Angle = .zero

    let minimumRotation = Angle.radians(2 * .pi * 0.02)
    struct ResizeRotateGestureState {
        var anchor: UnitPoint
        var location: CGPoint
        var angle: Angle = .zero
        var scale: CGFloat = 1.0
    }
    @GestureState private var resizeRotateGestureState: ResizeRotateGestureState?
    private var resizeRotateGesture: some Gesture {
        SimultaneousGesture(
            MagnifyGesture(minimumScaleDelta: scale * 0.01),
            RotateGesture(minimumAngleDelta: minimumRotation)
        )
        .updating($resizeRotateGestureState) { svalue, state, _ in
            let value = ResizeRotateGestureValue(svalue)
            state = state ?? .init(anchor: value.anchor, location: value.location)
            state?.angle ?= value.rotation
            state?.scale ?= value.magnification
        }
        .onEnded { svalue in
            let value = ResizeRotateGestureValue(svalue)

            let oldAngle = angle
            let oldScale = scale
            if let rotation = value.rotation {
                angle += rotation
            }
            if let magnification = value.magnification {
                scale *= magnification
            }

            if let newScrollPosition = calculateScrollPosition(
                anchor: value.location,
                oldScale: oldScale,
                oldAngle: oldAngle,
                newScale: scale,
                newAngle: angle
            ) {
                scrollPosition.scrollTo(point: newScrollPosition)
            }
        }
    }

    /// Calculates the scroll position needed to keep an anchor point at the same viewport location
    /// after applying scale and rotation transforms.
    ///
    /// This method maps the anchor point through coordinate transformations:
    /// bounding box → transformed content → original content → new transformed content → new bounding box
    ///
    /// - Parameters:
    ///   - anchor: The anchor point in viewport coordinates that should remain fixed
    ///   - oldScale: The scale factor before the transform
    ///   - oldAngle: The rotation angle before the transform
    ///   - newScale: The scale factor after the transform
    ///   - newAngle: The rotation angle after the transform
    /// - Returns: The new scroll position, or nil if required geometry is unavailable
    private func calculateScrollPosition(
        anchor: CGPoint,
        oldScale: CGFloat,
        oldAngle: Angle,
        newScale: CGFloat,
        newAngle: Angle,
    ) -> CGPoint? {
        guard let defaultContentSize, let contentFrame, let viewportFrame else {
            return nil
        }

        // Step 1: Find anchor position within the current content frame (bounding box)
        let anchorInContentFrame = (anchor - contentFrame.origin).point

        // Step 2: Calculate the old bounding box offset
        // When we transform and take bounding box, the origin shifts
        let oldTransform = CGAffineTransform(scale: oldScale, rotation: oldAngle, anchor: .zero)
        let oldBoundingBox = CGRect(origin: .zero, size: defaultContentSize).applying(oldTransform)
        let oldBoundingBoxOffset = (-oldBoundingBox.origin.vector).point

        // Step 3: Map anchor from bounding box to actual transformed content coordinates
        let anchorInOldTransformed = anchorInContentFrame - oldBoundingBoxOffset.vector

        // Step 4: Map back to original content coordinates
        let anchorInOriginal = anchorInOldTransformed.applying(oldTransform.inverted())

        // Step 5: Apply new transform to get new transformed content coordinates
        let newTransform = CGAffineTransform(scale: newScale, rotation: newAngle, anchor: .zero)
        let anchorInNewTransformed = anchorInOriginal.applying(newTransform)

        // Step 6: Calculate new bounding box offset
        let newBoundingBox = CGRect(origin: .zero, size: defaultContentSize).applying(newTransform)
        let newBoundingBoxOffset = (-newBoundingBox.origin.vector).point

        // Step 7: Map from transformed content to new bounding box coordinates
        let anchorInNewContentFrame = anchorInNewTransformed + newBoundingBoxOffset.vector

        // Step 8: Calculate where content origin should be to keep anchor at same viewport position
        let newContentOrigin = (anchor - anchorInNewContentFrame).point

        // Scroll position is the negative of content origin (when content is scrolled)
        var newScrollPosition = CGPoint(
            x: max(0, -newContentOrigin.x),
            y: max(0, -newContentOrigin.y)
        )

        // Check if content is smaller than viewport
        let newContentSize = newBoundingBox.size.absoluteValue

        if newContentSize.width <= viewportFrame.size.width {
            newScrollPosition.x = 0
        }
        if newContentSize.height <= viewportFrame.size.height {
            newScrollPosition.y = 0
        }

        return newScrollPosition
    }

    private func applyZoomState(zoomState: ZoomState) {
        guard let viewportSize = viewportFrame?.size else { return }
        guard let defaultContentSize else { return }
        switch zoomState {
        case .fit:
            scale = min(viewportSize.width / defaultContentSize.width,  viewportSize.height / defaultContentSize.height)
            angle = .zero
        case .fill:
            scale = max(viewportSize.width / defaultContentSize.width, viewportSize.height / defaultContentSize.height)
            angle = .zero
        case .actualSize:
            scale = 1.0
            angle = .zero
        }
    }


    private var doubleTapGesture: some Gesture {
        TapGesture(count: 2).onEnded {
            applyZoomState(zoomState: nextZoomState)
            nextZoomState = nextZoomState.next
        }
    }

    /// Next zoom state to use for double tap
    @State var nextZoomState: ZoomState = .fit

    @State var didInitialZoom: Bool = false
    @State var defaultContentSize: CGSize?
    @State var viewportFrame: CGRect?
    @State var scrollPosition = ScrollPosition()
    /// The frame of the content after permanent transformations in the scroll view's coordinate space
    @State var contentFrame: CGRect?
    @State var contentOffset: CGPoint?

    var body: some View {
        ScrollView([.horizontal, .vertical]) {
            ZoomedRotatedView(
                content:
                    content.onGeometryChange(
                        for: CGSize.self,
                        of: \.size,
                        action: { defaultContentSize = $0 }
                    ),
                scale: scale,
                rotation: angle
            )
            .onGeometryChange(
                for: CGRect.self,
                of: { proxy in proxy.frame(in: .named("ZoomableView-viewport")) },
                action: { contentFrame = $0 }
            )
            .modifierIfLet(resizeRotateGestureState) { state in
                _ScaleEffect(
                    scale: CGSize(square: state.scale),
                    anchor: UnitPoint(state.location, relativeTo: contentFrame ?? .zero)
                )
                .ignoredByLayout()
            }
            .modifierIfLet(resizeRotateGestureState) { state in
                _RotationEffect(
                    angle: state.angle,
                    anchor: UnitPoint(state.location, relativeTo: contentFrame ?? .zero)
                )
                .ignoredByLayout()
            }
        }
        .onGeometryChange(
            for: CGRect.self,
            of: { proxy in proxy.frame(in: .named("Zoomable-viewport"))},
            action: { viewportFrame = $0 }
        )
        .onScrollGeometryChange(
            for: CGPoint.self,
            of: { proxy in
                return proxy.contentOffset
            },
            action: {
                contentOffset = $1
            }
        )
        .scrollPosition($scrollPosition)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .gesture(resizeRotateGesture)
        .gesture(doubleTapGesture)
        .id("ZoomableView-viewport")
        .onAppear {
            if !didInitialZoom {
                applyZoomState(zoomState: nextZoomState)
                nextZoomState = nextZoomState.next
                didInitialZoom = true
            }
        }
    }
}

private struct ResizeRotateGestureValue {
    let anchor: UnitPoint
    let location: CGPoint
    let magnification: CGFloat?
    let rotation: Angle?

    init(_ value: SimultaneousGesture<MagnifyGesture, RotateGesture>.Value) {
        assert(value.first?.startAnchor == nil
               || value.second?.startAnchor == nil
               || value.first?.startAnchor == value.second?.startAnchor,
               "in a simultaneous gesture the anchors produced by these gestures should agree"
        )
        guard let anchor = value.first?.startAnchor ?? value.second?.startAnchor else {
            fatalError("at least one of the gestures must be started")
        }
        assert(value.first?.startLocation == nil
               || value.second?.startLocation == nil
               || value.first?.startLocation == value.second?.startLocation,
               "in a simultaneous gesture the locations produced by these gestures should agree"
        )
        guard let location = value.first?.startLocation ?? value.second?.startLocation else {
            fatalError("at least one of the gestures must be started")
        }
        self.anchor = anchor
        self.location = location
        self.magnification = value.first?.magnification
        self.rotation = value.second?.rotation
    }
}

#if DEBUG
#Preview {
    ZoomableView {
        Image(uiImage: .millionLive11)
    }
    .ignoresSafeArea()
}
#endif
