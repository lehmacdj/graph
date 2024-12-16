//
//  NID.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/24.
//

private let nidDigits = 12

struct NID: Equatable, Hashable {
    let representation: String

    init?(representation: String) {
        guard representation.count == nidDigits && representation.allSatisfy({$0.isBase62Digit}) else {
            return nil
        }
        self.representation = representation
    }

    /// Fake NID constructed from an int.
    init(fake input: Int) {
        var representation = ""
        precondition(nidDigits < 11)
        var remainder = input % pow(input, nidDigits)

        while representation.count < nidDigits {
            let ix = String.base62Digits.index(String.base62Digits.startIndex, offsetBy: remainder % 62)
            let char = String.base62Digits[ix]
            representation.append(String(char))
            remainder /= 62
        }

        self.init(representation: representation)!
    }
}

extension NID: Encodable, Decodable {
    public init(from decoder: Decoder) throws {
        let representation = try String(from: decoder)
        if let nid = NID(representation: representation) {
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

extension NID {
    var metaPath: String { return "\(representation).json" }
    var dataPath: String { return "\(representation).data" }
}

extension NID {
    static func random() -> NID {
        let randomString = String((0..<nidDigits).map { _ in String.base62Digits.randomElement()! })
        return NID(representation: randomString)!
    }
}

extension NID: CustomStringConvertible {
    var description: String {
        return "@\(representation)"
    }
}

