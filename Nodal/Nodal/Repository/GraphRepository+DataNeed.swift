//
//  DataNeed.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/2/25.
//

import Foundation

enum UntypedDataNeed: Comparable {
    case dataNotNeeded
    case wantDataIfLocal
    case needDataEvenIfRemote
}

enum LocalDataValue {
    case noData
    case localData(Data)
    case remoteDataExists

    /// For @dynamicMemberLookup syntactic sugar
    var data: Self { self }
}

enum UntypedDataValue {
    case dataNotChecked
    case dataIfLocal(LocalDataValue)
    case data(Data?)

    /// For @dynamicMemberLookup syntactic sugar
    var data: Self { self }
}

struct DataValue {
    let data: Data?
}

protocol DataNeed {
    associatedtype Value
    var untyped: UntypedDataNeed { get }
    func coerceValue(_ value: UntypedDataValue) -> Value?
}

struct DataNotNeeded: DataNeed {
    let untyped = UntypedDataNeed.dataNotNeeded
    func coerceValue(_ value: UntypedDataValue) -> Void? {
        if case .dataNotChecked = value {
            ()
        } else {
            nil
        }
    }
}

struct WantDataIfLocal: DataNeed {
    let untyped = UntypedDataNeed.wantDataIfLocal
    func coerceValue(_ value: UntypedDataValue) -> LocalDataValue? {
        if case .dataIfLocal(let localDataValue) = value {
            localDataValue
        } else {
            nil
        }
    }
}

struct NeedDataEvenIfRemote: DataNeed {
    let untyped = UntypedDataNeed.needDataEvenIfRemote
    func coerceValue(_ value: UntypedDataValue) -> DataValue? {
        if case .data(let data) = value {
            DataValue(data: data)
        } else {
            nil
        }
    }
}

extension DataNeed where Self == DataNotNeeded {
    static var dataNotNeeded: DataNotNeeded { .init() }
}

extension DataNeed where Self == WantDataIfLocal {
    static var wantDataIfLocal: WantDataIfLocal { .init() }
}

extension DataNeed where Self == NeedDataEvenIfRemote {
    static var needDataEvenIfRemote: NeedDataEvenIfRemote { .init() }
}

