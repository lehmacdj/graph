//
//  NID.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/24.
//

private let nidDigits = 12

struct NID: Equatable, Hashable {
    let underlying: Base62Id
    var representation: String { underlying.representation }

    struct InvalidNID: Error {
        let digitCount: Int
    }

    init(underlying: Base62Id) throws {
        guard underlying.digitCount == nidDigits else {
            throw InvalidNID(digitCount: underlying.digitCount)
        }
        self.underlying = underlying
    }

    init?(representation: String) {
        guard let underlying = Base62Id(representation: representation) else { return nil }
       try? self.init(underlying: underlying)
    }

    /// Fake NID constructed from an int.
    init(fake input: Int) {
        try! self.init(underlying: Base62Id(digitCount: nidDigits, fake: input))
    }
}

extension NID: Encodable, Decodable {
    init(from decoder: any Decoder) throws {
        try self.init(underlying: try .init(from: decoder))
    }

    func encode(to encoder: Encoder) throws {
        try underlying.encode(to: encoder)
    }
}

extension NID {
    var metaPath: String { return "\(representation).json" }
    var dataPath: String { return "\(representation).data" }
}

extension NID {
    static func random() -> NID {
        try! .init(underlying: Base62Id.random(digitCount: nidDigits))
    }
}

extension NID: CustomStringConvertible {
    var description: String {
        return "@\(representation)"
    }
}

