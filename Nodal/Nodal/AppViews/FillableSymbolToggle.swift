//
//  TagToggle.swift
//  Nodal
//
//  Created by Devin Lehmacher on 4/8/23.
//

import SwiftUI
import AsyncButton

struct FillableSymbolToggle: View {
    let isActive: Bool
    let fillableSymbol: FillableSymbol
    let action: (() async -> Void)?

    init(isActive: Bool, fillableSymbol: FillableSymbol, action: (() async -> Void)? = nil) {
        self.isActive = isActive
        self.fillableSymbol = fillableSymbol
        self.action = action
    }

    var body: some View {
        AsyncButton {
            await action?()
        } label: {
            if isActive {
                Image(systemName: fillableSymbol.filled)
            } else {
                Image(systemName: fillableSymbol.unfilled)
            }
        }
    }
}

struct TagToggler_Previews: PreviewProvider {
    static var previews: some View {
        VStack {
            FillableSymbolToggle(isActive: true, fillableSymbol: .trash)
            FillableSymbolToggle(isActive: false, fillableSymbol: .trash)
        }.previewLayout(.sizeThatFits)
    }
}
