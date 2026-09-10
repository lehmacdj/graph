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

    /// Name of the coordinate space the viewport establishes. All geometry
    /// used for anchoring gestures and computing scroll positions is measured
    /// in this space so that it agrees with gesture locations, which are
    /// reported relative to the view the gesture is attached to.
    private static var viewportSpace: String { "ZoomableView-viewport" }

    @State private var scale: CGFloat = 1.0
    @State private var angle: Angle = .zero

    let minimumRotation = Angle.radians(2 * .pi * 0.02)
    /// `MagnifyGesture.magnification` is relative to the start of the gesture,
    /// so the threshold to begin recognizing must not depend on the current
    /// zoom. Scaling it by `scale` made the gesture progressively harder to
    /// start the further you zoomed in, until pinching stopped working at all.
    let minimumScaleDelta: CGFloat = 0.01
    struct ResizeRotateGestureState {
        var anchor: UnitPoint
        var location: CGPoint
        var angle: Angle = .zero
        var scale: CGFloat = 1.0
    }
    @GestureState private var resizeRotateGestureState: ResizeRotateGestureState?
    private var resizeRotateGesture: some Gesture {
        SimultaneousGesture(
            MagnifyGesture(minimumScaleDelta: minimumScaleDelta),
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
            if let rotation = value.rotation, rotation.radians.isFinite {
                angle += rotation
            }
            if let magnification = value.magnification, magnification.isFinite {
                scale = clampedScale(scale * magnification)
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

    /// The scale at which the untransformed content exactly fits the viewport,
    /// or nil while the geometry needed to compute it is unavailable.
    private var fitScale: CGFloat? {
        guard let viewportSize = viewportFrame?.size, let defaultContentSize else {
            return nil
        }
        guard defaultContentSize.width > 0, defaultContentSize.height > 0 else {
            // The content hasn't been laid out yet (or has no intrinsic size).
            // Dividing here would produce an infinite scale that then poisons
            // every subsequent transform.
            return nil
        }
        return min(
            viewportSize.width / defaultContentSize.width,
            viewportSize.height / defaultContentSize.height
        )
    }

    /// Largest scale we allow zooming to.
    ///
    /// Scaling the content up produces a correspondingly large layer for the
    /// render server to rasterize, so without a bound a few pinches can grow
    /// the content to a size that stalls the app.
    private var maximumScale: CGFloat {
        let absoluteMaximum: CGFloat = 32
        let maximumRenderedSide: CGFloat = 16384
        guard let defaultContentSize else { return absoluteMaximum }
        let longestSide = max(defaultContentSize.width, defaultContentSize.height)
        guard longestSide > 0 else { return absoluteMaximum }
        // Never bound below the scale needed to fit, otherwise a very large
        // image couldn't be shown in its entirety.
        return max(fitScale ?? 0, min(absoluteMaximum, maximumRenderedSide / longestSide))
    }

    /// Smallest scale we allow zooming to; there's no point in shrinking the
    /// content much beyond the point where it fits the viewport.
    private var minimumScale: CGFloat {
        min(fitScale ?? 1, 1) / 4
    }

    private func clampedScale(_ scale: CGFloat) -> CGFloat {
        guard scale.isFinite, scale > 0 else { return self.scale }
        return min(max(scale, minimumScale), maximumScale)
    }

    /// Applies a zoom state, returning whether it could be applied. It can't be
    /// applied until the viewport and content have both been measured.
    @discardableResult
    private func applyZoomState(zoomState: ZoomState) -> Bool {
        guard let viewportSize = viewportFrame?.size, let defaultContentSize, let fitScale else {
            return false
        }
        switch zoomState {
        case .fit:
            scale = clampedScale(fitScale)
        case .fill:
            scale = clampedScale(max(
                viewportSize.width / defaultContentSize.width,
                viewportSize.height / defaultContentSize.height
            ))
        case .actualSize:
            scale = clampedScale(1.0)
        }
        angle = .zero
        return true
    }

    /// The anchor, relative to the content, that an in-progress gesture should
    /// scale and rotate the content about. Falls back to the center of the
    /// content while the content hasn't been measured yet.
    private func gestureAnchor(at location: CGPoint) -> UnitPoint {
        guard let contentFrame else { return .center }
        return UnitPoint(location, relativeTo: contentFrame)
    }

    /// Performs the initial zoom to fit as soon as the geometry required to do
    /// so is available.
    ///
    /// This can't happen in `onAppear`: that runs before the first geometry is
    /// reported, so the zoom would silently do nothing and the content would be
    /// left at actual size.
    private func applyInitialZoomStateIfPossible() {
        guard !didInitialZoom else { return }
        guard applyZoomState(zoomState: nextZoomState) else { return }
        nextZoomState = nextZoomState.next
        didInitialZoom = true
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
                        action: {
                            defaultContentSize = $0
                            applyInitialZoomStateIfPossible()
                        }
                    ),
                scale: scale,
                rotation: angle
            )
            .onGeometryChange(
                for: CGRect.self,
                of: { proxy in proxy.frame(in: .named(Self.viewportSpace)) },
                action: { contentFrame = $0 }
            )
            .modifierIfLet(resizeRotateGestureState) { state in
                _ScaleEffect(
                    scale: CGSize(square: state.scale),
                    anchor: gestureAnchor(at: state.location)
                )
                .ignoredByLayout()
            }
            .modifierIfLet(resizeRotateGestureState) { state in
                _RotationEffect(
                    angle: state.angle,
                    anchor: gestureAnchor(at: state.location)
                )
                .ignoredByLayout()
            }
            // Keep the content centered when it's smaller than the viewport.
            // A ScrollView pins its content to the top-leading corner, so
            // without this a fit-scaled image smaller than the screen would
            // stick to the top-left instead of being centered.
            .frame(
                minWidth: viewportFrame?.width,
                minHeight: viewportFrame?.height,
                alignment: .center
            )
        }
        .onGeometryChange(
            for: CGRect.self,
            of: { proxy in proxy.frame(in: .named(Self.viewportSpace)) },
            action: {
                viewportFrame = $0
                applyInitialZoomStateIfPossible()
            }
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
        .simultaneousGesture(resizeRotateGesture)
        .gesture(doubleTapGesture)
        .coordinateSpace(.named(Self.viewportSpace))
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
