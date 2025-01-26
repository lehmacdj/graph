//
//  Logging+ExtraContext.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/5/25.
//

protocol LogContextProviding {
    func logContext(isolation: isolated (any Actor)?) -> [String]
}

extension LogContextProviding {
    func logContext(isolation: isolated (any Actor)? = #isolation) -> [String] {
        logContext(isolation: isolation)
    }
}

extension LogContextProviding {
    func log(
        _ message: String,
        level: LogLevel,
        file: String = #file,
        line: Int = #line,
        column: Int = #column,
        callingFunction: String = #function,
        isolation: isolated (any Actor)? = #isolation
    ) {
        Nodal.log(
            message,
            level: level,
            file: file,
            line: line,
            column: column,
            context: logContext(isolation: isolation),
            callingFunction: callingFunction
        )
    }

    func logDebug(
        _ message: String,
        file: String = #file,
        line: Int = #line,
        column: Int = #column,
        callingFunction: String = #function,
        isolation: isolated (any Actor)? = #isolation
    ) {
        log(
            message,
            level: .debug,
            file: file,
            line: line,
            column: column,
            callingFunction: callingFunction,
            isolation: isolation
        )
    }

    func logInfo(
        _ message: String,
        file: String = #file,
        line: Int = #line,
        column: Int = #column,
        callingFunction: String = #function,
        isolation: isolated (any Actor)? = #isolation
    ) {
        log(
            message,
            level: .info,
            file: file,
            line: line,
            column: column,
            callingFunction: callingFunction,
            isolation: isolation
        )
    }


    func logError(
        _ message: String,
        file: String = #file,
        line: Int = #line,
        column: Int = #column,
        callingFunction: String = #function,
        isolation: isolated (any Actor)? = #isolation
    ) {
        log(
            message,
            level: .error,
            file: file,
            line: line,
            column: column,
            callingFunction: callingFunction,
            isolation: isolation
        )
    }

    func logError(
        _ error: Error,
        file: String = #file,
        line: Int = #line,
        column: Int = #column,
        callingFunction: String = #function,
        isolation: isolated (any Actor)? = #isolation
    ) {
        log(
            String(reflecting: error),
            level: .error,
            file: file,
            line: line,
            column: column,
            callingFunction: callingFunction,
            isolation: isolation
        )
    }

    func trace<T>(
        _ val: T,
        level: LogLevel = .debug,
        file: String = #file,
        line: Int = #line,
        column: Int = #column,
        callingFunction: String = #function,
        isolation: isolated (any Actor)? = #isolation
    ) -> T {
        log(
            String(reflecting: val),
            level: .debug,
            file: file,
            line: line,
            column: column,
            callingFunction: callingFunction,
            isolation: isolation
        )
        return val
    }
}

struct FrozenLogger: LogContextProviding {
    let logContext: [String]
}

extension LogContextProviding {
    /// Returns a logger with context frozen to the context from the current context.
    /// This does not capture/retain self.
    func frozenLogger(
        file: StaticString = #file,
        line: UInt = #line,
        column: UInt = #column,
        isolation: isolated (any Actor)? = #isolation
    ) -> FrozenLogger {
        FrozenLogger(
            logContext: frozenContext(
                file: file,
                line: line,
                column: column,
                isolation: isolation
            )
        )
    }

    func frozenContext(
        file: StaticString = #file,
        line: UInt = #line,
        column: UInt = #column,
        isolation: isolated (any Actor)? = #isolation
    ) -> [String] {
        ["frozen@\(file):\(line):\(column)"] + logContext(isolation: isolation)
    }
}

extension Optional where Wrapped: LogContextProviding {
    func frozenLogger(
        file: StaticString = #file,
        line: UInt = #line,
        column: UInt = #column,
        isolation: isolated (any Actor)? = #isolation
    ) -> FrozenLogger {
        FrozenLogger(
            logContext:
                self?.frozenContext(
                    file: file,
                    line: line,
                    column: column,
                    isolation: isolation
                )
                ?? ["freeze-context-nil@\(file):\(line):\(column)"]
        )
    }
}
