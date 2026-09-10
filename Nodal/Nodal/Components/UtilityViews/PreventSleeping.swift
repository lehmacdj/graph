//
//  PreventSleeping.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/22/25.
//

import SwiftUI

struct PreventSleeping: ViewModifier {
    var isActive = true

    func body(content: Content) -> some View {
        content
            .onAppear {
                if isActive {
                    UIApplication.shared.isIdleTimerDisabled = true
                }
            }
            .onChange(of: isActive) { _, isActive in
                UIApplication.shared.isIdleTimerDisabled = isActive
            }
            .onDisappear {
                if isActive {
                    UIApplication.shared.isIdleTimerDisabled = false
                }
            }
    }
}

extension View {
    func preventSleeping() -> some View {
        modifier(PreventSleeping())
    }

    /// Prevents sleeping while `shouldPreventSleeping` is true.
    ///
    /// Toggling this must not add or remove the modifier conditionally: that
    /// changes the structural identity of the view, recreating it and
    /// discarding its state (e.g. the zoom and scroll position of an image).
    func preventSleeping(_ shouldPreventSleeping: Bool) -> some View {
        modifier(PreventSleeping(isActive: shouldPreventSleeping))
    }
}
