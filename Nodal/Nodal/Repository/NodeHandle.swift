//
//  NodeHandle.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/24.
//

protocol NodeHandle: Actor {
    /// Acquire a stream of updates for a NodeValue + augmentations computed from that node. There is a new element of the `AsyncSequence` whenever the data underlying the `NodeValue<A>` changes.
    /// See ``Augmentation`` for more details on how augmentations work.
    func updates<A: Augmentation>(augmentedWith augmentation: A.Type) -> any AsyncSequence<NodeValue<A>, Never>
}

extension NodeHandle {
    func updates<A: Augmentation>(augmentedWith augmentation: A.Type = NoAugmentation.self) -> any AsyncSequence<NodeValue<A>, Never> {
        updates<A>(augmentedWith: augmentation)
    }
}
