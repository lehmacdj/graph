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
func log(_ message: String, level: LogLevel = .info, callingFunction: String = #function) {
    if level >= logLevel {
        print("[\(Date())][\(callingFunction)][\(level)]: \(message)")
    }
}

func debug(_ message: String, callingFunction: String = #function) {
    log(message, level: .debug, callingFunction: callingFunction)
}

func warn(_ message: String, callingFunction: String = #function) {
    log(message, level: .warning, callingFunction: callingFunction)
}

func error(_ message: String, callingFunction: String = #function) {
    log(message, level: .error, callingFunction: callingFunction)
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
