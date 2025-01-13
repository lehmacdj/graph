//
//  Node+NoAugmentation.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/12/25.
//

/// Struct for having equatable `NodeValue`s sans an augmentation
/// Can't use `Void` because it doesn't have `Equatable` conformance
struct NoAugmentation: Equatable, Hashable {}

extension Node<NoAugmentation> {
    init(id: NID, outgoing: [String:Set<NID>], incoming: [String:Set<NID>]) {
        self.init(id: id, outgoing: outgoing, incoming: incoming, augmentation: NoAugmentation())
    }
}
