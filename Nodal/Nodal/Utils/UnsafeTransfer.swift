//
//  UnsafeTransfer.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/26/25.
//

struct UnsafeTransfer<T>: @unchecked Sendable {
    let unsafelySending: T
}
