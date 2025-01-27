//
//  TagToggle.swift
//  Nodal
//
//  Created by Devin Lehmacher on 4/8/23.
//

import SwiftUI

struct FillableSymbolToggle: View {
    let isActive: Bool
    let fillableSymbol: FillableSymbol
    let action: (() -> Void)?

    init(isActive: Bool, fillableSymbol: FillableSymbol, action: (() -> Void)? = nil) {
        self.isActive = isActive
        self.fillableSymbol = fillableSymbol
        self.action = action
    }

    var body: some View {
        Button {
            action?()
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
