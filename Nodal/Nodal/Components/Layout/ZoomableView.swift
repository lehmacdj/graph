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
            state = state ?? .init(anchor: value.anchor)
            state?.angle ?= value.rotation
            state?.scale ?= value.magnification
        }
        .onEnded { svalue in
            let value = ResizeRotateGestureValue(svalue)
            let oldAngle = angle
            let oldScale = scale
            // Apply the transformations
            if let rotation = value.rotation {
                angle += rotation
            }
            if let magnification = value.magnification {
                scale *= magnification
            }
        }
    }

    private func applyZoomState(zoomState: ZoomState) {
        guard let viewportSize else { return }
        guard let defaultContentSize else { return }
        logDebug("viewportSize: \(viewportSize), contentSize: \(defaultContentSize)")
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
    @State var viewportSize: CGSize?
    @State var scrollPosition = ScrollPosition()
    /// The frame of the content after permanent transformations in the scroll view's coordinate space
    @State var contentFrame: CGRect?

    func getContentActualPoint(scrollViewPoint: UnitPoint) -> CGPoint? {
        guard let viewportSize = viewportSize,
              let contentFrame = contentFrame else {
            return nil
        }
        
        // Convert scroll view unit point to actual point in viewport coordinates
        let scrollViewActualPoint = CGPoint(
            x: scrollViewPoint.x * viewportSize.width,
            y: scrollViewPoint.y * viewportSize.height
        )
        
        // Convert viewport point to content frame coordinates
        let contentActualPoint = CGPoint(
            x: scrollViewActualPoint.x - contentFrame.minX,
            y: scrollViewActualPoint.y - contentFrame.minY
        )
        
        return contentActualPoint
    }
    
    func contentActualPointToUnitPoint(contentActualPoint: CGPoint) -> UnitPoint? {
        guard let contentFrame = contentFrame else {
            return nil
        }
        
        // Convert to unit point within the content frame
        let contentUnitPoint = UnitPoint(
            x: contentFrame.width > 0 ? contentActualPoint.x / contentFrame.width : 0,
            y: contentFrame.height > 0 ? contentActualPoint.y / contentFrame.height : 0
        )

        return contentUnitPoint
    }
    
    func getContentUnitPoint(scrollViewPoint: UnitPoint) -> UnitPoint {
        guard let contentActualPoint = getContentActualPoint(scrollViewPoint: scrollViewPoint),
              let contentUnitPoint = contentActualPointToUnitPoint(contentActualPoint: contentActualPoint) else {
            return scrollViewPoint // fallback if sizes aren't available yet
        }
        
        return contentUnitPoint
    }

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
                of: { proxy in
                    proxy.frame(in: .named("ZoomableView-viewport"))
                },
                action: {
                    logDebug("contentFrame: \($0)")
                    contentFrame = $0
                }
            )
            .border(Color.pink)
            .modifierIfLet(resizeRotateGestureState) { state in
                _ScaleEffect(
                    scale: CGSize(square: state.scale),
                    anchor: getContentUnitPoint(scrollViewPoint: state.anchor)
                )
                .ignoredByLayout()
            }
            .modifierIfLet(resizeRotateGestureState) { state in
                _RotationEffect(
                    angle: state.angle,
                    anchor: getContentUnitPoint(scrollViewPoint: state.anchor)
                )
                .ignoredByLayout()
            }
        }
        .onGeometryChange(
            for: CGSize.self,
            of: \.size,
            action: { viewportSize = $0 }
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
        self.anchor = anchor
        self.magnification = value.first?.magnification
        self.rotation = value.second?.rotation
    }
}

#Preview {
    ZoomableView {
        Image(uiImage: .millionLive11)
    }
    .ignoresSafeArea()
}
