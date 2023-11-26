//
//  Utils.swift
//  view
//
//  Created by Devin Lehmacher on 4/7/21.
//

// Utility functions here, so want them to stick around most likely.
// periphery:ignore:all

import Foundation
import SwiftUI

/// Log level to use when logging traces via log function
let logLevel = LogLevel.verbose

/// Logging function to be abstracted out into a real logging framework later if that seems worth it
func log(_ message: String, level: LogLevel = .info, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) {
    if level >= logLevel {
        print("[\(Date())][\(file):\(line):\(column)][\(callingFunction)][\(level)]: \(message)")
    }
}

func logDebug(_ message: String, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) {
    log(message, level: .debug, file: file, callingFunction: callingFunction, line: line, column: column)
}

func logInfo(_ message: String, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) {
    log(message, level: .info, file: file, callingFunction: callingFunction, line: line, column: column)
}

func logWarn(_ message: String, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) {
    log(message, level: .warning, file: file, callingFunction: callingFunction, line: line, column: column)
}

func logError(_ message: String, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) {
    log(message, level: .error, file: file, callingFunction: callingFunction, line: line, column: column)
}

func trace<T>(_ val: T, level: LogLevel = .info, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) -> T {
    log(String(reflecting: val), level: .info, file: file, callingFunction: callingFunction, line: line, column: column)
    return val
}

enum LogLevel: Int, Equatable {
    case error = 1
    case warning = 2
    case info = 3
    case debug = 4
    case verbose = 5
}

extension LogLevel: Comparable {
    static func < (lhs: LogLevel, rhs: LogLevel) -> Bool {
        // log level is less if it is less urgent
        return lhs.rawValue > rhs.rawValue
    }
}

func mutate<T>(_ value: T, mutator: (inout T) -> ()) -> T {
    var value = value
    mutator(&value)
    return value
}

extension View {
    @ViewBuilder
    func modifyIfLet<T>(_ value: T?, modifier: (T) -> some ViewModifier) -> some View {
        if let value {
            self.modifier(modifier(value))
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
        return String(self.absoluteString.dropFirst(self.absoluteString.count))
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
