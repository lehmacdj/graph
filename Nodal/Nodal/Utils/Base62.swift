//
//  Base62.swift
//  Nodal
//
//  Created by Devin Lehmacher on 6/22/24.
//

import Foundation

extension Character {
    var isBase62Digit: Bool {
        isASCII && (isLetter || isNumber)
    }
}

extension String {
    static let base62Digits: String =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

}
