//
//  Logging.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/5/25.
//

import Foundation

/// Log level to use when logging traces via log function
let logLevel = LogLevel.debug

/// Logging function to be abstracted out into a real logging framework later if that seems worth it
func log(_ message: String, level: LogLevel = .info, file: String = #file, line: Int = #line, column: Int = #column, context: [String] = [], callingFunction: String = #function) {
    if level >= logLevel {
        let contextString = context
            .map { "[\($0)]" }
            .joined(separator: "")
        print("[\(Date())][\(level)][\(file):\(line):\(column)]\(contextString)[\(callingFunction)]: \(message)")
    }
}

func logDebug(_ message: String, level: LogLevel = .debug, file: String = #file, line: Int = #line, column: Int = #column, context: [String] = [], callingFunction: String = #function) {
    log(message, level: .debug, file: file, line: line, column: column, context: context, callingFunction: callingFunction)
}

func logInfo(_ message: String, level: LogLevel = .info, file: String = #file, line: Int = #line, column: Int = #column, context: [String] = [], callingFunction: String = #function) {
    log(message, level: .info, file: file, line: line, column: column, context: context, callingFunction: callingFunction)
}

func logWarn(_ message: String, level: LogLevel = .warning, file: String = #file, line: Int = #line, column: Int = #column, context: [String] = [], callingFunction: String = #function) {
    log(message, level: .warning, file: file, line: line, column: column, context: context, callingFunction: callingFunction)
}

func logError(_ message: String, file: String = #file, line: Int = #line, column: Int = #column, context: [String] = [], callingFunction: String = #function) {
    log(message, level: .error, file: file, line: line, column: column, context: context, callingFunction: callingFunction)
}

func logError(_ error: Error, file: String = #file, line: Int = #line, column: Int = #column, context: [String] = [], callingFunction: String = #function) {
    logError("\(error)", file: file, line: line, column: column, context: context, callingFunction: callingFunction)
}

func trace<T>(_ val: T, level: LogLevel = .info, file: String = #file, line: Int = #line, column: Int = #column, context: [String] = [], callingFunction: String = #function) -> T {
    log(String(reflecting: val), level: level, file: file, line: line, column: column, context: context, callingFunction: callingFunction)
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
