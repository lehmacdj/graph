//
//  Geometry.swift
//  Nodal
//
//  Created by Devin Lehmacher on 7/16/23.
//

import Foundation

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

    var vector: CGPoint {
        CGPoint(x: width, y: height)
    }

    init(square side: CGFloat) {
        self.init(width: side, height: side)
    }
}

extension CGPoint {
    static func -(lhs: CGPoint, rhs: CGPoint) -> CGSize {
        CGSize(width: lhs.x - rhs.x, height: lhs.y - rhs.y)
    }

    static func -(lhs: CGPoint, rhs: CGSize) -> CGPoint {
        CGPoint(x: lhs.x - rhs.width, y: lhs.y - rhs.height)
    }

    var size: CGSize {
        CGSize(width: x, height: y)
    }
}

extension CGRect {
    var center: CGPoint {
        CGPoint(x: midX, y: midY)
    }
}
