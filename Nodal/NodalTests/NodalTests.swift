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
}
