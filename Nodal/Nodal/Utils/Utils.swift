//
//  Utils.swift
//  Nodal
//
//  Created by Devin Lehmacher on 4/7/21.
//

// Utility functions here, so want them to stick around most likely.
// periphery:ignore:all

import Foundation
import SwiftUI


func mutate<T>(_ value: T, mutator: (inout T) -> ()) -> T {
    var value = value
    mutator(&value)
    return value
}

extension View {
    @ViewBuilder
    func modifierIfLet<T>(_ value: T?, modifier: (T) -> some ViewModifier) -> some View {
        if let value {
            self.modifier(modifier(value))
        } else {
            self
        }
    }

    @ViewBuilder
    func modifyIfLet<T>(_ value: T?, modify: (T, Self) -> some View) -> some View {
        if let value {
            modify(value, self)
        } else {
            self
        }
    }
}

extension URL {
    /// Return the remainder of the URL after removing the given prefix or nil if the prefix is not a prefix of this URL
    func trimmingPath(prefix url: URL) -> String? {
        guard self.absoluteString.starts(with: url.absoluteString) else {
            return nil
        }
        return String(self.absoluteString.dropFirst(url.absoluteString.count))
    }
}

extension String {
    /// Trims the given string off the end of the string or returns nil if the string does not occur at the end of the string
    func trimming(suffix: String) -> String? {
        guard self.hasSuffix(suffix) else {
            return nil
        }
        return String(self.dropLast(suffix.count))
    }
}

extension Sequence {
    /// For constructing things with postfix notation, e.g. `[1, 2, 3].map { $0 * 2 }.to(Array.init)`
    func to<T>(_ constructor: (Self) -> T) -> T {
        constructor(self)
    }
}

extension Result {
    var isSuccess: Bool {
        switch self {
        case .success: return true
        case .failure: return false
        }
    }
}
