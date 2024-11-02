//
//  ConnectDTO.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/24.
//

struct ConnectDTO: Decodable, Encodable {
    let transition: String
    let id: NID
    enum CodingKeys: String, CodingKey {
        case transition = "t"
        case id = "n"
    }
}
