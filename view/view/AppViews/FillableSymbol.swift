//
//  FillableSymbol.swift
//  view
//
//  Created by Devin Lehmacher on 4/23/23.
//

import Foundation

enum FillableSymbol: String, CaseIterable {
    case bookmark = "bookmark"
    case trash = "trash"
    case star = "star"
    case xmarkBin = "xmark.bin"

    var unfilled: String {
        rawValue
    }

    var filled: String {
        rawValue + ".fill"
    }
}
