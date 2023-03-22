//
//  Utils.swift
//  view
//
//  Created by Devin Lehmacher on 4/7/21.
//

import Foundation

/// Log level to use when logging traces via log function
let logLevel = LogLevel.verbose

/// Logging function to be abstracted out into a real logging framework later if that seems worth it
func log(_ message: String, level: LogLevel = .info, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) {
    if level >= logLevel {
        print("[\(Date())][\(file):\(line):\(column)][\(callingFunction)][\(level)]: \(message)")
    }
}

func debug(_ message: String, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) {
    log(message, level: .debug, file: file, callingFunction: callingFunction, line: line, column: column)
}

func info(_ message: String, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) {
    log(message, level: .info, file: file, callingFunction: callingFunction, line: line, column: column)
}

func warn(_ message: String, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) {
    log(message, level: .warning, file: file, callingFunction: callingFunction, line: line, column: column)
}

func error(_ message: String, file: String = #file, callingFunction: String = #function, line: Int = #line, column: Int = #column) {
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
