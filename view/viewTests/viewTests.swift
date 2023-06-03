//
//  viewTests.swift
//  viewTests
//
//  Created by Devin Lehmacher on 10/10/21.
//

import XCTest
import SwiftUI
@testable import view

class viewTests: XCTestCase {
    func testFillableSymbolsExist() throws {
        for symbol in FillableSymbol.allCases {
            // if an SFSymbol exists it's possible to create an image using it
            XCTAssertNotNil(Image(systemName: symbol.rawValue))
        }
    }
}
