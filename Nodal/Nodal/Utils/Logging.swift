//
//  Logging.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/5/25.
//

import Foundation

/// Log level to use when logging traces via log function
let logLevel = LogLevel.debug

@TaskLocal var logContext: [String] = []

func withPushLogContext<T>(_ context: String..., operation: () throws -> T) rethrows {
    try $logContext.withValue(logContext + context, operation: operation)
}

func withPushLogContext<T>(_ context: String..., operation: () async throws -> T) async rethrows {
    try await $logContext.withValue(logContext + context, operation: operation)
}

/// Logging function to be abstracted out into a real logging framework later if that seems worth it
func log(
    _ message: String,
    level: LogLevel = .info,
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    if level >= logLevel {
        let timestamp = "\(Date())"
        let caller = "\(file):\(line):\(column):\(callingFunction)"
        let threadInfo = if Thread.current.isMainThread {
            "thread:main"
        } else {
            "thread:\(Thread.current.hashValue)"
        }
        let context = [
            timestamp,
            "\(level)",
            threadInfo,
            caller,
        ] + logContext
        let contextString = context
            .map { "[\($0)]" }
            .joined(separator: "")
        print("\(contextString): \(message)")
    }
}

func logDebug(
    _ message: String,
    level: LogLevel = .debug,
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    log(
        message,
        level: .debug,
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
}

func logInfo(
    _ message: String,
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    log(
        message,
        level: .info,
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
}

func logWarn(
    _ message: String,
    level: LogLevel = .warning,
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    log(
        message,
        level: .warning,
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
}

func logError(
    _ message: String,
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    log(
        message,
        level: .error,
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
}

func logError(
    _ error: Error,
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    context: [String] = [],
    callingFunction: String = #function
) {
    logError(
        "\(error)",
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
}

func trace<T>(
    _ val: T,
    level: LogLevel = .debug,
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) -> T {
    log(
        String(reflecting: val),
        level: level,
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
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
