//
//  NodalTests.swift
//  NodalTests
//
//  Created by Devin Lehmacher on 10/10/21.
//

import XCTest
import SwiftUI
@testable import Nodal

class NodalTests: XCTestCase {
    func testFillableSymbolsExist() throws {
        for symbol in FillableSymbol.allCases {
            // if an SFSymbol exists it's possible to create an image using it
            XCTAssertNotNil(Image(systemName: symbol.rawValue))
        }
    }


    func testWeakDictionaryDeallocates() {
        class Foo {
            let number: Int
            init(_ number: Int) {
                self.number = number
            }
        }

        var x = WeakDictionary<Int, Foo>()
        var x0: Foo? = Foo(0)
        x[0] = x0
        x[1] = Foo(1)

        XCTAssertEqual(x.count, 1)
        x0 = nil
        XCTAssertEqual(x.count, 0)
    }

    /// means we only caring about microseconds, we need to round due to unavoidable floating point arithmetic
    let nanosecondPowerOf10Precision = 3

    func testParseTime() throws {
        let actual = Calendar.iso8601.dateComponents(
            [.hour, .minute, .second, .nanosecond],
            from: try XCTUnwrap(parseTime(string: "04:14:28.417274000000"))
        )
        XCTAssertEqual(actual.hour, 4)
        XCTAssertEqual(actual.minute, 14)
        XCTAssertEqual(actual.second, 28)
        XCTAssertEqual(
            actual.nanosecond?.rounded(toPowerOf10: nanosecondPowerOf10Precision),
            417274000.rounded(toPowerOf10: nanosecondPowerOf10Precision)
        )
    }

    func testMakeDate() throws {
        let actual = Calendar.iso8601.dateComponents(
            [.year, .month, .day, .hour, .minute, .second, .nanosecond],
            from: try XCTUnwrap(makeDate(
                year: 2021,
                month: 12,
                day: 31,
                time: try XCTUnwrap(parseTime(string: "04:14:28.417274000000"))
            ))
        )
        XCTAssertEqual(actual.year, 2021)
        XCTAssertEqual(actual.month, 12)
        XCTAssertEqual(actual.day, 31)
        XCTAssertEqual(actual.hour, 4)
        XCTAssertEqual(actual.minute, 14)
        XCTAssertEqual(actual.second, 28)
        XCTAssertEqual(
            actual.nanosecond?.rounded(toPowerOf10: nanosecondPowerOf10Precision),
            417274000.rounded(toPowerOf10: nanosecondPowerOf10Precision)
        )
    }
}
