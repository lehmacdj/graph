//
//  GraphTimestamp.swift
//  Nodal
//
//  Created by Devin Lehmacher on 5/12/24.
//

import Foundation
import AsyncAlgorithms

private struct GraphYearComponent: Codable, CustomDebugStringConvertible {
    let year: Int

    init(yearString: String) throws {
        guard let year = Int(yearString), yearString.count == 4 else {
            throw NSError(domain: "GraphYearComponent", code: 1, userInfo: [NSLocalizedDescriptionKey: "Year must be a 4-digit integer"])
        }
        self.year = year
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        let yearString = try container.decode(String.self)
        try self.init(yearString: yearString)
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(String(format: "%04d", year))
    }

    var debugDescription: String {
        "\(year)"
    }
}

private struct GraphMonthComponent: Codable, CustomDebugStringConvertible {
    let month: Int

    init(monthString: String) throws {
        guard let month = Int(monthString), monthString.count == 2, month >= 1, month <= 12 else {
            throw NSError(domain: "GraphMonthComponent", code: 1, userInfo: [NSLocalizedDescriptionKey: "Month must be a 2-digit integer between 01 and 12"])
        }
        self.month = month
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        let monthString = try container.decode(String.self)
        try self.init(monthString: monthString)
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(String(format: "%02d", month))
    }

    var debugDescription: String {
        "\(month)"
    }
}

private struct GraphDayComponent: Codable, CustomDebugStringConvertible {
    let day: Int

    init(dayString: String) throws {
        guard let day = Int(dayString), dayString.count == 2, day >= 1, day <= 31 else {
            throw NSError(domain: "GraphDayComponent", code: 1, userInfo: [NSLocalizedDescriptionKey: "Day must be a 2-digit integer between 01 and 31"])
        }
        self.day = day
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        let dayString = try container.decode(String.self)
        try self.init(dayString: dayString)
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(String(format: "%02d", day))
    }

    var debugDescription: String {
        "\(day)"
    }
}

private struct GraphTimeComponent: Codable, CustomDebugStringConvertible {
    let time: Date

    // Custom initializer accepting a string
    init(timeString: String) throws {
        guard let parsedTime = parseTime(string: timeString) else {
            throw NSError(domain: "GraphTimeComponent", code: 1, userInfo: [NSLocalizedDescriptionKey: "Time string is not in the expected format"])
        }
        self.time = parsedTime
    }

    // Decode using the single value container
    init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        let timeString = try container.decode(String.self)
        try self.init(timeString: timeString)
    }

    // Encode using the single value container
    func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        let dateFormatter = DateFormatter()
        dateFormatter.dateFormat = "HH:mm:ss.SSSSSSSSS"
        let dateString = dateFormatter.string(from: time)
        try container.encode(dateString)
    }

    var debugDescription: String {
        "\(time.formatted(date: .omitted, time: .complete))"
    }
}

func parseTime(string: String) -> Date? {
    let stringParts = string.split(separator: /\./)
    guard stringParts.count == 2 else {
        return nil
    }

    let nonFractional = stringParts[0]
    let fractionalSeconds = stringParts[1]

    let dateFormatter = DateFormatter()
    dateFormatter.dateFormat = "HH:mm:ss"
    guard let time = dateFormatter.date(from: String(nonFractional)) else {
        return nil
    }

    guard let nanoseconds = Int(fractionalSeconds.padding(toLength: 9, withPad: "0", startingAt: 0).prefix(9)) else {
        return nil
    }
    let nanoInterval = TimeInterval(nanoseconds) / 1e9
    return time.addingTimeInterval(nanoInterval)
}

func makeDate(year: Int, month: Int, day: Int, time: Date) -> Date {
    let calendar = Calendar(identifier: .iso8601)
    let timeComponents = calendar.dateComponents(
        [.hour, .minute, .second, .nanosecond],
        from: time
    )
    let dateComponents = DateComponents(
        calendar: calendar,
        timeZone: .gmt,
        year: year,
        month: month,
        day: day,
        hour: timeComponents.hour,
        minute: timeComponents.minute,
        second: timeComponents.second,
        nanosecond: timeComponents.nanosecond
    )
    assert(dateComponents.isValidDate)
    return dateComponents.date!
}

struct GraphTimestampBuilder {
    private var time: GraphTimeComponent?
    private var day: GraphDayComponent?
    private var month: GraphMonthComponent?
    private var year: GraphYearComponent?

    func parseTime(from string: String) -> GraphTimestampBuilder? {
        guard time == nil else {
            logWarn("tried to parse time twice")
            return nil
        }
        guard let timeComponent = try? GraphTimeComponent(timeString: string) else {
            return nil
        }
        var result = self
        result.time = timeComponent
        return result
    }

    func parseDay(from string: String) -> GraphTimestampBuilder? {
        guard day == nil else {
            logWarn("tried to parse day twice")
            return nil
        }
        guard let dayComponent = try? GraphDayComponent(dayString: string) else {
            return nil
        }
        var result = self
        result.day = dayComponent
        return result
    }

    func parseMonth(from string: String) -> GraphTimestampBuilder? {
        guard month == nil else {
            logWarn("tried to parse month twice")
            return nil
        }
        guard let monthComponent = try? GraphMonthComponent(monthString: string) else {
            return nil
        }
        var result = self
        result.month = monthComponent
        return result
    }

    func parseYear(from string: String) -> GraphTimestampBuilder? {
        guard year == nil else {
            logWarn("tried to parse year twice")
            return nil
        }
        guard let yearComponent = try? GraphYearComponent(yearString: string) else {
            return nil
        }
        var result = self
        result.year = yearComponent
        return result
    }

    func build() -> Date? {
        guard let year = year?.year,
          let month = month?.month,
          let day = day?.day,
          let time = time?.time else {
            return nil
        }
        return makeDate(year: year, month: month, day: day, time: time)
    }
}
