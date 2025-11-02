//
//  DataNeed.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/2/25.
//

import Foundation

typealias DataAvailability = FileAvailability

struct DataWithURL {
    let data: Data
    let url: URL
}

enum DataNeed: Comparable {
    case dataNotNeeded
    case needsAvailabilityOnly
    case wantDataIfLocal
    case needDataEvenIfRemote
}

enum LocalDataValue {
    case noData
    case localData(DataWithURL)
    case remoteDataExists(URL)

    /// For @dynamicMemberLookup syntactic sugar
    var data: Self { self }
}

enum UntypedDataValue {
    case dataNotChecked
    case availabilityOnly(DataAvailability)
    case dataIfLocal(LocalDataValue)
    case data(DataWithURL?)

    /// For @dynamicMemberLookup syntactic sugar
    var data: Self { self }
}

struct DataValue {
    let data: DataWithURL?
}

protocol TypedDataNeed {
    associatedtype Value
    var untyped: DataNeed { get }
    func coerceValue(_ value: UntypedDataValue) -> Value?
}

struct DataNotNeeded: TypedDataNeed {
    let untyped = DataNeed.dataNotNeeded
    func coerceValue(_ value: UntypedDataValue) -> NoAugmentation? {
        if case .dataNotChecked = value {
            NoAugmentation()
        } else {
            nil
        }
    }
}

struct NeedsAvailabilityOnly: TypedDataNeed {
    let untyped = DataNeed.needsAvailabilityOnly
    func coerceValue(_ value: UntypedDataValue) -> DataAvailability? {
        if case .availabilityOnly(let availability) = value {
            availability
        } else {
            nil
        }
    }
}

struct WantDataIfLocal: TypedDataNeed {
    let untyped = DataNeed.wantDataIfLocal
    func coerceValue(_ value: UntypedDataValue) -> LocalDataValue? {
        if case .dataIfLocal(let localDataValue) = value {
            localDataValue
        } else {
            nil
        }
    }
}

struct NeedDataEvenIfRemote: TypedDataNeed {
    let untyped = DataNeed.needDataEvenIfRemote
    func coerceValue(_ value: UntypedDataValue) -> DataValue? {
        if case .data(let data) = value {
            DataValue(data: data)
        } else {
            nil
        }
    }
}

extension TypedDataNeed where Self == DataNotNeeded {
    static var dataNotNeeded: DataNotNeeded { .init() }
}

extension TypedDataNeed where Self == NeedsAvailabilityOnly {
    static var needsAvailabilityOnly: NeedsAvailabilityOnly { .init() }
}

extension TypedDataNeed where Self == WantDataIfLocal {
    static var wantDataIfLocal: WantDataIfLocal { .init() }
}

extension TypedDataNeed where Self == NeedDataEvenIfRemote {
    static var needDataEvenIfRemote: NeedDataEvenIfRemote { .init() }
}

