//
//  BottomOrnament.swift
//  Nodal
//
//  Created by Devin Lehmacher on 5/12/24.
//

import SwiftUI

/// Displays a view below another view while keeping the frame of the first view
struct BottomOrnament<Content: View, Ornament: View>: View {
    let content: () -> Content
    let ornament: () -> Ornament

    init(
        @ViewBuilder content: @escaping () -> Content,
        @ViewBuilder ornament: @escaping () -> Ornament
    ) {
        self.content = content
        self.ornament = ornament
    }

    var body: some View {
        content()
            .anchorPreference(key: OrnamentedBoundsPreferenceKey.self, value: .bounds, transform: { $0 })
            .overlayPreferenceValue(OrnamentedBoundsPreferenceKey.self) { preferences in
                GeometryReader { proxy in
                    preferences.map {
                        ornament()
                            .offset(y: proxy[$0].height)
                    }
                }
            }
    }
}

struct OrnamentedBoundsPreferenceKey: PreferenceKey {
    typealias Value = Anchor<CGRect>?

    static var defaultValue: Value = nil

    static func reduce(
        value: inout Value,
        nextValue: () -> Value
    ) {
        value = nextValue()
    }
}

#Preview {
    BottomOrnament {
        Text("Hello world!")
    } ornament: {
        Text("Ornament")
    }
    .border(Color.red)
}
