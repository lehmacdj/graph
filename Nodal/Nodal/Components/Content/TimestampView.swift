//
//  TimestampView.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/26/25.
//

import SwiftUI

struct TimestampView: View {
    @Environment(\.dateProvider) var dateProvider

    let timestamp: Date

    private var formattedTimestamp: String {
        let now = dateProvider.now
        let components = Calendar.iso8601.dateComponents([.minute, .hour], from: timestamp, to: now)

        if let minutes = components.minute,
           let hours = components.hour,
           hours.magnitude < 24
        {
            switch () {
            case () where hours == 0 && minutes < 0:
                return minutes == -1 ? "in 1 min" : "in \(-minutes) mins"
            case () where hours == 0 && minutes == 0:
                return "now"
            case () where hours == 0 && minutes > 0:
                return minutes == 1 ? "1 min ago" : "\(minutes) mins ago"
            case () where hours < 0:
                return hours == -1 ? "in 1 hour" : "in \(-hours) hours"
            case () where hours > 0:
                return hours == 1 ? "1 hour ago" : "\(hours) hours ago"
            default:
                break
            }
        }

        let formatter = DateFormatter()
        formatter.dateFormat = "yyyy-MM-dd"
        return formatter.string(from: timestamp)
    }

    var body: some View {
        Text(formattedTimestamp)
    }
}

#if DEBUG
private let referenceDate = Date(timeIntervalSince1970: 1706300400) // 2024-01-26 20:00:00 UTC

#Preview{
    Group {
        TimestampView(timestamp: referenceDate.addingTimeInterval(1))     // now
        TimestampView(timestamp: referenceDate.addingTimeInterval(-1 * 60))     // 1 min ago
        TimestampView(timestamp: referenceDate.addingTimeInterval(-5 * 60))     // 5 mins ago
        TimestampView(timestamp: referenceDate.addingTimeInterval(-65 * 60))    // 1 hour 5 mins ago
        TimestampView(timestamp: referenceDate.addingTimeInterval(-23 * 3600))  // 23 hours ago
        TimestampView(timestamp: referenceDate.addingTimeInterval(-48 * 3600))  // 2 days ago
        TimestampView(timestamp: referenceDate.addingTimeInterval(1 * 60))  // in 1 min
        TimestampView(timestamp: referenceDate.addingTimeInterval(3 * 60))  // in 3 mins
        TimestampView(timestamp: referenceDate.addingTimeInterval(75 * 60))  // in 1 hour
        TimestampView(timestamp: referenceDate.addingTimeInterval(13 * 3600))  // in 13 hours
        TimestampView(timestamp: referenceDate.addingTimeInterval(25 * 3600))  // in 1 day 1 hours
    }
    .mockingDateProvider(referenceDate)
}
#endif
