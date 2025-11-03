//
//  GeometryTests.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/25.
//

@testable import Nodal
import SwiftUI
import Testing

@Test(
    arguments: [
        (
            UnitPoint.topLeading,
            CGRect(origin: .zero, size: CGSize(width: 2, height: 2)),
            CGPoint(x: 0, y: 0)
        ),
        (
            .topTrailing,
            CGRect(origin: .zero, size: CGSize(width: 2, height: 2)),
            CGPoint(x: 2, y: 0)
        ),
        (
            .center,
            CGRect(origin: CGPoint(x: 2, y: 2), size: CGSize(width: 2, height: 2)),
            CGPoint(x: 3, y: 3)
        ),
        (
            .center,
            CGRect(origin: CGPoint(x: 2, y: 2), size: CGSize(width: 3, height: 4)),
            CGPoint(x: 3.5, y: 4)
        )

    ]
)
func unitPointPointConversion(args: (UnitPoint, CGRect, CGPoint)) {
    let (unitPoint, rect, point) = args
    #expect(unitPoint.in(frame: rect) == point)
    #expect(UnitPoint(point, relativeTo: rect) == unitPoint)
}
