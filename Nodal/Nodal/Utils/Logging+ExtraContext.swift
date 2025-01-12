//
//  Logging+ExtraContext.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/5/25.
//

protocol LogContextProviding {
    var logContext: [String] { get }
}

extension LogContextProviding {
    func log(_ message: String, level: LogLevel = .info, file: String = #file, line: Int = #line, column: Int = #column, callingFunction: String = #function) {
        Nodal.log(message, level: level, file: file, line: line, column: column, context: logContext, callingFunction: callingFunction)
    }

    func logDebug(_ message: String, level: LogLevel = .debug, file: String = #file, line: Int = #line, column: Int = #column, callingFunction: String = #function) {
        Nodal.log(message, level: .debug, file: file, line: line, column: column, context: logContext, callingFunction: callingFunction)
    }

    func logInfo(_ message: String, level: LogLevel = .info, file: String = #file, line: Int = #line, column: Int = #column, callingFunction: String = #function) {
        Nodal.log(message, level: .info, file: file, line: line, column: column, context: logContext, callingFunction: callingFunction)
    }

    func logWarn(_ message: String, level: LogLevel = .warning, file: String = #file, line: Int = #line, column: Int = #column, callingFunction: String = #function) {
        Nodal.log(message, level: .warning, file: file, line: line, column: column, context: logContext, callingFunction: callingFunction)
    }

    func logError(_ message: String, file: String = #file, line: Int = #line, column: Int = #column, callingFunction: String = #function) {
        Nodal.log(message, level: .error, file: file, line: line, column: column, context: logContext, callingFunction: callingFunction)
    }

    func logError(_ error: Error, file: String = #file, line: Int = #line, column: Int = #column, callingFunction: String = #function) {
        Nodal.logError("\(error)", file: file, line: line, column: column, context: logContext, callingFunction: callingFunction)
    }

    func trace<T>(_ val: T, level: LogLevel = .info, file: String = #file, line: Int = #line, column: Int = #column, callingFunction: String = #function) -> T {
        Nodal.log(String(reflecting: val), level: level, file: file, line: line, column: column, context: logContext, callingFunction: callingFunction)
        return val
    }
}
