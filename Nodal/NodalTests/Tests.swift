//
//  Tests.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/21/25.
//

@testable import Nodal
import Testing

@Test(
    arguments: [
        (0, "0000000000"),
        (1, "0000000001"),
        (91, "000000001T"),
        (839_299_365_868_340_223, "zzzzzzzzzz"),
    ]
)
func fakeNID(_ n: Int, representation: String) {
    #expect(Base62Id(digitCount: 10, fake: n).representation == representation)
}
