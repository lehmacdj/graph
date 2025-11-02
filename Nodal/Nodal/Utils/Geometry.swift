//
//  Geometry.swift
//  Nodal
//
//  Created by Devin Lehmacher on 7/16/23.
//

import Foundation
import SwiftUI

extension CGSize {
    static func +(lhs: CGSize, rhs: CGSize) -> CGSize {
        CGSize(width: lhs.width + rhs.width, height: lhs.height + rhs.height)
    }

    static func -(lhs: CGSize, rhs: CGSize) -> CGSize {
        CGSize(width: lhs.width - rhs.width, height: lhs.height - rhs.height)
    }

    static func +=(lhs: inout CGSize, rhs: CGSize) {
        lhs = lhs + rhs
    }

    static func -=(lhs: inout CGSize, rhs: CGSize) {
        lhs = lhs - rhs
    }

    static func /(lhs: CGSize, rhs: CGFloat) -> CGSize {
        CGSize(width: lhs.width / rhs, height: lhs.height / rhs)
    }

    static func *(lhs: CGSize, rhs: CGFloat) -> CGSize {
        CGSize(width: lhs.width * rhs, height: lhs.height * rhs)
    }

    var vector: CGVector {
        CGVector(dx: width, dy: height)
    }

    var point: CGPoint {
        CGPoint(x: width, y: height)
    }

    init(square side: CGFloat) {
        self.init(width: side, height: side)
    }
}

extension CGPoint {
    static func +(lhs: CGPoint, rhs: CGVector) -> CGPoint {
        CGPoint(x: lhs.x + rhs.dx, y: lhs.y + rhs.dy)
    }

    static func -(lhs: CGPoint, rhs: CGPoint) -> CGVector {
        CGVector(dx: lhs.x - rhs.x, dy: lhs.y - rhs.y)
    }

    static func -(lhs: CGPoint, rhs: CGVector) -> CGPoint {
        CGPoint(x: lhs.x - rhs.dx, y: lhs.y - rhs.dy)
    }

    var size: CGSize {
        CGSize(width: x, height: y)
    }

    var vector: CGVector {
        CGVector(dx: x, dy: y)
    }
}

extension CGVector {
    static func *(lhs: CGVector, rhs: CGFloat) -> CGVector {
        CGVector(dx: lhs.dx * rhs, dy: lhs.dy * rhs)
    }

    static func +(lhs: CGVector, rhs: CGVector) -> CGVector {
        CGVector(dx: lhs.dx + rhs.dy, dy: lhs.dy + rhs.dy)
    }

    static func -(lhs: CGVector, rhs: CGVector) -> CGVector {
        CGVector(dx: lhs.dx - rhs.dx, dy: lhs.dy - rhs.dy)
    }

    static func /(lhs: CGVector, rhs: CGFloat) -> CGVector {
        CGVector(dx: lhs.dx / rhs, dy: lhs.dy - rhs)
    }

    var point: CGPoint {
        CGPoint(x: dx, y: dy)
    }

    var size: CGSize {
        CGSize(width: dx, height: dy)
    }
}

extension CGRect {
    var center: CGPoint {
        CGPoint(x: midX, y: midY)
    }

    static func +(lhs: CGRect, rhs: CGVector) -> CGRect {
        CGRect(origin: lhs.origin + rhs, size: lhs.size)
    }

    static func -(lhs: CGRect, rhs: CGVector) -> CGRect {
        CGRect(origin: lhs.center - rhs, size: lhs.size)
    }
}

extension UnitPoint {
    func `in`(frame: CGRect) -> CGPoint {
        CGPoint(x: x * frame.width, y: y * frame.height) + frame.origin.vector
    }

    init(_ point: CGPoint, relativeTo rect: CGRect) {
        let opoint = point - rect.origin.vector
        self.init(x: opoint.x / rect.size.width, y: opoint.y / rect.height)
    }
}
