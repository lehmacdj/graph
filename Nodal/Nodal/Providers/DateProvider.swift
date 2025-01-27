//
//  DateProvider.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/26/25.
//

import Foundation
import SwiftUI

protocol DateProvider: Sendable {
    @MainActor
    var now: Date { get }
}

@Observable
final class LiveDateProvider: DateProvider, @unchecked Sendable {
    @MainActor
    private(set) var now: Date = Date.now

    @ObservationIgnored private var refreshTask: Task<Void, Never>?

    init(refreshInterval: Duration = .seconds(60)) {
        refreshTask = Task { @MainActor [refreshInterval, weak self] in
            while !Task.isCancelled {
                try? await Task.sleep(for: refreshInterval)
                self?.now = Date.now
            }
        }
    }

    deinit {
        refreshTask?.cancel()
    }
}

private struct _EnvironmentKey: EnvironmentKey {
    static let defaultValue: DateProvider = LiveDateProvider()
}

extension EnvironmentValues {
    var dateProvider: DateProvider {
        get { self[_EnvironmentKey.self] }
        set { self[_EnvironmentKey.self] = newValue }
    }
}

struct MockDateProvider: DateProvider {
    let now: Date

    init(now: Date) {
        self.now = now
    }
}

extension View {
    func mockingDateProvider(_ date: Date = .init(timeIntervalSince1970: 0)) -> some View {
        environment(\.dateProvider, MockDateProvider(now: date))
    }

    func withDateProvider(refreshInterval: Duration) -> some View {
        environment(\.dateProvider, LiveDateProvider(refreshInterval: refreshInterval))
    }
}
