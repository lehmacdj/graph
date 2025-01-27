//
//  PreventSleeping.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/22/25.
//

import SwiftUI

struct PreventSleeping: ViewModifier {
    func body(content: Content) -> some View {
        content
            .onAppear {
                UIApplication.shared.isIdleTimerDisabled = true
            }
            .onDisappear {
                UIApplication.shared.isIdleTimerDisabled = false
            }
    }
}

extension View {
    func preventSleeping() -> some View {
        modifier(PreventSleeping())
    }

    @ViewBuilder
    func preventSleeping(_ shouldPreventSleeping: Bool) -> some View {
        if shouldPreventSleeping {
            modifier(PreventSleeping())
        } else {
            self
        }
    }
}
