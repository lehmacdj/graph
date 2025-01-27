//
//  SortOrderIcon.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/26/25.
//

import SwiftUI

struct SortOrderIcon: View {
    let sortOrder: NodeSortOrder

    var body: some View {
        switch sortOrder {
        case .transitionThenTimestamp(.newerFirst):
            Image(systemName: "character")
        case .timestampThenTransition(.newerFirst):
            Image(systemName: "clock")
                .overlay {
                    Image(systemName: "arrow.down")
                        .resizable()
                        .frame(width: 8, height: 10)
                        .offset(x: 6, y: -6)
                }
        case .transitionThenTimestamp(.olderFirst):
            Image(systemName: "character")
                .overlay {
                    Image(systemName: "arrow.up")
                        .resizable()
                        .frame(width: 8, height: 10)
                        .offset(x: 6, y: -6)
                }
        case .timestampThenTransition(.olderFirst):
            Image(systemName: "clock")
                .overlay {
                    Image(systemName: "arrow.up")
                        .resizable()
                        .frame(width: 8, height: 10)
                        .offset(x: 6, y: -6)
                }
        }
    }
}

#Preview {
    SortOrderIcon(sortOrder: .transitionThenTimestamp(timestampOrder: .newerFirst))
    SortOrderIcon(sortOrder: .timestampThenTransition(timestampOrder: .newerFirst))
    SortOrderIcon(sortOrder: .transitionThenTimestamp(timestampOrder: .olderFirst))
    SortOrderIcon(sortOrder: .timestampThenTransition(timestampOrder: .olderFirst))
}
