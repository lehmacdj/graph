//
//  ZoomableView.swift
//  Nodal
//
//  Created by Devin Lehmacher on 7/14/23.
//

import SwiftUI

enum ZoomState {
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

    // handle scaleAnchor & rotateAnchor to make scaling and rotating better
    struct ScaleInfo {
        let scale: CGFloat
        let anchor: UnitPoint
    }
    @GestureState private var currentScaleInfo: ScaleInfo?
    var magnificationGesture: some Gesture {
        MagnifyGesture(minimumScaleDelta: 0.01 * scale)
            .updating($currentScaleInfo) { value, gestureState, _ in
                gestureState = ScaleInfo(scale: value.magnification, anchor: value.startAnchor)
            }
            .onEnded { value in scale *= value.magnification }
    }

    let minimumRotation = Angle.radians(2 * .pi * 0.02)
    struct RotationInfo {
        let angle: Angle
        let anchor: UnitPoint
    }
    @GestureState private var currentRotationInfo: RotationInfo?
    private var rotationGesture: some Gesture {
        RotateGesture(minimumAngleDelta: minimumRotation)
            .updating($currentRotationInfo) { value, gestureState, _ in
                gestureState = RotationInfo(angle: value.rotation, anchor: value.startAnchor)
            }
            .onEnded { value in
                angle += value.rotation
            }
    }

    private func applyZoomState(zoomState: ZoomState, viewSize: CGSize) {
        switch zoomState {
        case .fit:
            scale = min(viewSize.width / contentSize.width,  viewSize.height / contentSize.height)
            angle = .zero
        case .fill:
            scale = max(viewSize.width / contentSize.width, viewSize.height / contentSize.height)
            angle = .zero
        case .actualSize:
            scale = 1.0
            angle = .zero
        }
    }


    private func doubleTapGesture(_ viewSize: CGSize) -> some Gesture {
        TapGesture(count: 2).onEnded {
            applyZoomState(zoomState: nextZoomState, viewSize: viewSize)
            nextZoomState = nextZoomState.next
        }
    }

    /// Next zoom state to use for double tap
    @State var nextZoomState: ZoomState = .fit

    @State var didInitialZoom: Bool = false
    @State var contentSize: CGSize = .zero

    var body: some View {
        GeometryReader { outerGeometry in
            ScrollView([.horizontal, .vertical]) {
                Color.clear
                    .frame(width: contentSize.width * scale, height: contentSize.height * scale)
                    .anchorPreference(key: ContentCenterPreference.self, value: .center) {
                        outerGeometry[$0]
                    }
            }
            .backgroundPreferenceValue(ContentCenterPreference.self) { contentCenter in
                content
                    .fixedSize()
                    .anchorPreference(key: ContentSizePreference.self, value: .bounds) {
                        outerGeometry[$0].size
                    }
                    .modifyIfLet(currentRotationInfo) { rotationInfo in
                        _RotationEffect(angle: rotationInfo.angle, anchor: rotationInfo.anchor).ignoredByLayout()
                    }
                    .rotationEffect(angle)
                    .modifier(_ScaleEffect(scale: CGSize(square: scale)).ignoredByLayout())
                    .modifyIfLet(currentScaleInfo) { scaleInfo in
                        _ScaleEffect(
                            scale: CGSize(square: scaleInfo.scale),
                            anchor: scaleInfo.anchor
                        )
                        .ignoredByLayout()
                    }
                    .offset(contentCenter - (outerGeometry.size / 2).vector)
            }
            .onPreferenceChange(ContentSizePreference.self) {
                contentSize = $0
                if !didInitialZoom {
                    applyZoomState(zoomState: nextZoomState, viewSize: outerGeometry.size)
                    nextZoomState = nextZoomState.next
                    didInitialZoom = true
                }
            }
            // it seems like the magnify gesture on it's own breaks when simultaneous with the rotation gesture; maybe worth trying to use SimultaneousGesture explicitly in order to resolve the conflict somehow
            .gesture(rotationGesture)
            .simultaneousGesture(magnificationGesture)
            .gesture(doubleTapGesture(outerGeometry.size))
        }
    }
}

struct ContentSizePreference: PreferenceKey {
    typealias Value = CGSize

    static var defaultValue: CGSize = .zero

    static func reduce(value: inout CGSize, nextValue: () -> CGSize) {
        value = nextValue()
    }
}


struct ContentCenterPreference: PreferenceKey {
    typealias Value = CGPoint

    static var defaultValue: CGPoint = .zero

    static func reduce(value: inout CGPoint, nextValue: () -> CGPoint) {
        value = nextValue()
    }
}

struct ZoomableView_Previews: PreviewProvider {
    static var previews: some View {
        ZoomableView {
            Rectangle().fill(Gradient(colors: [.red, .blue]))
                .frame(width: 100, height: 500)
        }
        .ignoresSafeArea()
    }
}
