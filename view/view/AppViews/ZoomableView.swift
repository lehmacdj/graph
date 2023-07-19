//
//  ZoomableView.swift
//  view
//
//  Created by Devin Lehmacher on 7/14/23.
//

import SwiftUI

struct ZoomableView<Content: View>: View {
    private let content: Content

    init(content: () -> Content) {
        self.content = content()
    }

    @State private var scale: CGFloat = 1.0
    @State private var angle: Angle = .zero

    @GestureState private var gestureScale: CGFloat = 1.0
    var magnificationGesture: some Gesture {
        MagnifyGesture(minimumScaleDelta: 0.01)
            .updating($gestureScale) { value, gestureState, _ in
                gestureState = value.magnification
            }
            .onEnded { value in scale *= value.magnification }
    }

    @GestureState private var gestureAngle: Angle = .zero
    private var rotationGesture: some Gesture {
        RotateGesture(minimumAngleDelta: .radians(2 * .pi * 0.01))
            .updating($gestureAngle) { value, gestureState, _ in
                gestureState = value.rotation
            }
            .onEnded { value in angle += value.rotation }
    }

    @State var contentSize: CGSize = .zero

    var body: some View {
        GeometryReader { outerGeometry in
            ScrollView([.horizontal, .vertical]) {
                Color.clear
                    .frame(width: contentSize.width, height: contentSize.height)
                    .anchorPreference(key: ContentCenterPreference.self, value: .center) {
                        outerGeometry[$0]
                    }
            }
            .gesture(rotationGesture)
            .simultaneousGesture(magnificationGesture)
            .backgroundPreferenceValue(ContentCenterPreference.self) { contentCenter in
                content
                    .fixedSize()
                    .anchorPreference(key: ContentSizePreference.self, value: .bounds) {
                        outerGeometry[$0].size
                    }
                    .modifier(_RotationEffect(angle: gestureAngle).ignoredByLayout())
                    .rotationEffect(angle)
                    .modifier(_ScaleEffect(scale: CGSize(square: gestureScale)).ignoredByLayout())
                    .scaleEffect(scale)
                    .offset(contentCenter - (outerGeometry.size / 2).vector)
            }
        }
        .onPreferenceChange(ContentSizePreference.self) {
            contentSize = $0
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
                .frame(idealWidth: 500, idealHeight: 500)
        }
        .scrollIndicators(.hidden)
    }
}
