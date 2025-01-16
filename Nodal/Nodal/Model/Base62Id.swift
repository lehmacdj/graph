//
//  Base62Id.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/5/25.
//

struct Base62Id: Equatable, Hashable {
    let representation: String
    var digitCount: Int { representation.count }

    init?(representation: String) {
        guard representation.allSatisfy({$0.isBase62Digit}) else {
            return nil
        }
        self.representation = representation
    }

    /// Fake NID constructed from an int.
    init(digitCount: Int, fake input: Int) {
        var representation = ""
        precondition(digitCount < 11)
        var remainder = input % (62 ** digitCount)

        while representation.count < digitCount {
            let ix = String.base62Digits.index(String.base62Digits.startIndex, offsetBy: remainder % 62)
            let char = String.base62Digits[ix]
            representation.append(String(char))
            remainder /= 62
        }

        self.init(representation: representation)!
    }

    static func random(digitCount: Int) -> Base62Id {
        let randomString = String((0..<digitCount).map { _ in String.base62Digits.randomElement()! })
        return Base62Id(representation: randomString)!
    }
}

extension Base62Id: Encodable, Decodable {
    public init(from decoder: Decoder) throws {
        let representation = try String(from: decoder)
        if let nid = Base62Id(representation: representation) {
            self = nid
        } else {
            throw DecodingError.dataCorrupted(
                .init(
                    codingPath: decoder.codingPath,
                    debugDescription: "Invalid NID representation: \(representation)"
                )
            )
        }
    }

    public func encode(to encoder: Encoder) throws {
        try representation.encode(to: encoder)
    }
}

extension Base62Id: CustomStringConvertible {
    var description: String {
        return "\(representation)"
    }
}

extension Base62Id {
    static func + (lhs: Base62Id, rhs: Base62Id) -> Base62Id {
        Base62Id(representation: lhs.representation + rhs.representation)!
    }
}
