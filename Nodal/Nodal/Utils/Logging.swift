//
//  Logging.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/5/25.
//

import Foundation

/// Log level to use when logging traces via log function
let logLevel = LogLevel.info

@TaskLocal var taskLogContext: [String] = []

func withPushLogContext<T>(_ context: String..., @_implicitSelfCapture operation: () throws -> T) rethrows {
    try $taskLogContext.withValue(taskLogContext + context, operation: operation)
}

func withPushLogContext<T>(_ context: String..., @_implicitSelfCapture operation: () async throws -> T) async rethrows {
    try await $taskLogContext.withValue(taskLogContext + context, operation: operation)
}

/// Logging function to be abstracted out into a real logging framework later if that seems worth it
func log(
    _ message: String,
    level: LogLevel = .info,
    extraLogContext: @autoclosure () -> [String] = [],
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    if level >= logLevel {
        let timestamp = "\(Date())"
        let caller = "\(file):\(line):\(column):\(callingFunction)"
//        let threadInfo = if Thread.current.isMainThread {
//            "thread:main"
//        } else {
//            "thread:\(Thread.current.hashValue)"
//        }
        let defaultLogContext = [
            timestamp,
            "\(level)",
            caller,
        ]
        let logContext = defaultLogContext
            + taskLogContext
            + extraLogContext()
        let contextString = logContext
            .map { "[\($0)]" }
            .joined(separator: "")
        print("\(contextString): \(message)")
    }
}

func logVerbose(
    _ message: String,
    extraLogContext: @autoclosure () -> [String] = [],
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    log(
        message,
        level: .verbose,
        extraLogContext: extraLogContext(),
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
}

func logDebug(
    _ message: String,
    extraLogContext: @autoclosure () -> [String] = [],
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    log(
        message,
        level: .debug,
        extraLogContext: extraLogContext(),
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
}

func logInfo(
    _ message: String,
    extraLogContext: @autoclosure () -> [String] = [],
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    log(
        message,
        level: .info,
        extraLogContext: extraLogContext(),
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
}

func logWarn(
    _ message: String,
    extraLogContext: @autoclosure () -> [String] = [],
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    log(
        message,
        level: .warning,
        extraLogContext: extraLogContext(),
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
}

func logError(
    _ message: String,
    extraLogContext: @autoclosure () -> [String] = [],
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    log(
        message,
        level: .error,
        extraLogContext: extraLogContext(),
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
}

func logError(
    _ error: Error,
    extraLogContext: @autoclosure () -> [String] = [],
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) {
    logError(
        "\(error)",
        extraLogContext: extraLogContext(),
        file: file,
        line: line,
        column: column,
        callingFunction: callingFunction
    )
}

func trace<T>(
    _ val: T,
    level: LogLevel = .debug,
    extraLogContext: @autoclosure () -> [String] = [],
    file: String = #file,
    line: Int = #line,
    column: Int = #column,
    callingFunction: String = #function
) -> T {
    log(
        String(reflecting: val),
        level: level,
        extraLogContext: extraLogContext(),
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
