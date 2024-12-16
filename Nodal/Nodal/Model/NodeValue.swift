//
//  NodeValue.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/4/24.
//

struct NodeValue<Augmentation>: Identifiable {
    let id: NID
    let outgoing: [String:Set<NID>]
    let incoming: [String:Set<NID>]
    let augmentation: Augmentation

    func withAugmentation<NewAugmentation>(_ newAugmentation: NewAugmentation) -> NodeValue<NewAugmentation> {
        NodeValue<NewAugmentation>(
            id: id,
            outgoing: outgoing,
            incoming: incoming,
            augmentation: newAugmentation
        )
    }
}

extension NodeValue {
    init(from metadata: NodeMeta, augmentation: Augmentation) {
        self.init(
            id: metadata.id,
            outgoing: metadata.outgoing,
            incoming: metadata.incoming,
            augmentation: augmentation
        )
    }
}

extension NodeValue<Void> {
    init(from metadata: NodeMeta) {
        self.init(from: metadata, augmentation: ())
    }
}
