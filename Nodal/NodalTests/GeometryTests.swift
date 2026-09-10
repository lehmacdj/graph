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

@Test func vectorArithmetic() {
    let a = CGVector(dx: 3, dy: 5)
    let b = CGVector(dx: 7, dy: 11)
    #expect(a + b == CGVector(dx: 10, dy: 16))
    #expect(b - a == CGVector(dx: 4, dy: 6))
    #expect(b * 2 == CGVector(dx: 14, dy: 22))
    #expect(CGVector(dx: 8, dy: 12) / 4 == CGVector(dx: 2, dy: 3))
}

@Test func rectCenterRoundTrip() {
    let rect = CGRect(center: CGPoint(x: 10, y: 20), size: CGSize(width: 4, height: 6))
    #expect(rect == CGRect(x: 8, y: 17, width: 4, height: 6))
    #expect(rect.center == CGPoint(x: 10, y: 20))
    #expect(rect - CGVector(dx: 8, dy: 17) == CGRect(x: 0, y: 0, width: 4, height: 6))
}

/// A degenerate rect has no unit space; falling back to the center keeps
/// infinities and NaNs out of the transforms these unit points anchor.
@Test func unitPointRelativeToDegenerateRect() {
    #expect(UnitPoint(CGPoint(x: 50, y: 25), relativeTo: .zero) == .center)
    #expect(
        UnitPoint(
            CGPoint(x: 50, y: 25),
            relativeTo: CGRect(x: 0, y: 0, width: 100, height: 0)
        ) == UnitPoint(x: 0.5, y: 0.5)
    )
}
