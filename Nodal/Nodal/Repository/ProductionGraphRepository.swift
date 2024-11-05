//
//  ProductionGraphRepository.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/4/24.
//

import Foundation

actor ProductionGraphRepository: GraphRepository {
    private let basePath: URL

    init(basePath: URL) {
        self.basePath = basePath
    }

    // MARK: GraphRepository

    typealias NodeHandle = ProductionNodeHandle

    func createNewNode() async -> NID {
        <#code#>
    }

    func getNode(withId id: NID) async throws -> ProductionNodeHandle {
        try await ProductionNodeHandle(basePath: basePath, id: id)
    }

    func deleteNode(withId id: NID) async {
        <#code#>
    }

    // there's a little bit of a problem here
    // we need to effectively atomically modify both the nodes referenced by from and to
    // we might need to switch to a model where only GraphRepository is an actor and all operations on nodes go through it
    // maybe we can have GraphRepository implement some kind of locking so that it is possible to acquire permission to write several nodes at the same time without worrying about intervening updates from other nodes?
    // if we implement such a lock though maybe NodeHandle doesn't need to be an actor anymore which would probably simplify things fairly significantly?
    func insertEdge(from: NID, to: NID) async {
        <#code#>
    }

    func deleteEdge(from: NID, to: NID) async {
        <#code#>
    }

    // MARK: Private

    fileprivate func getNodeValue(withId id: NID) -> NodeValue<Void> {
    }
}

actor ProductionNodeHandle: NodeHandle {
    private let basePath: URL
    private let id: NID

    // maybe we just move almost everything into GraphRepository and the handles just become references that do stuff on the GraphRepository on deinit? It seems practically impossible to make changes to one NodeMetadataDocument without also changing other ones given that we have backlinks
    // it seems like maybe we can have ProductionNodeHandle just be a struct; that way we can probably still use it as though isolated in some contexts
    // the idea of using a lock like thing in GraphRepository definitely is legit too though, that way we can still perform writes in parallel because the actual write actions are performed on different actors
    private var _metadataDocument: NodeMetadataDocument?

    init(basePath: URL, id: NID) async throws {
        self.basePath = basePath
        self.id = id
        // this is fine, but maybe we can get away with not initing NodeMetadataDocument until we subscribe to updates?
        // the problem is, we do need some type to act as the handle that owns/retains the NodeMetadataDocument
        self._metadataDocument = try await NodeMetadataDocument(metaURL: basePath.appendingPathComponent(id.metaPath))
    }

    deinit {
        Task { [_metadataDocument] in
            async let _ = _metadataDocument?.close()
        }
    }

    func updates<A: Augmentation>(augmentedWith augmentation: A.Type) -> any AsyncSequence<NodeValue<A>, Never> {
        // this'll be tricky
        // we probably need to define a custom AsyncIteratorProtocol type that hangs onto the NodeHandles of all of the dependencies computed on the prior iteration
        // we listen for updates on all of those nodes and then when any of them gets updated we recompute the new value + augmentation and also recompute dependencies to account for new/broken dependencies as a result of the change in more complicated augmentations (e.g. timestamp)
        // ownershipwise the iterator can probably hold on to everything strongly because as long as the sequence is being consumed we don't want to deallocate any of the NodeHandles that are dependencies anyways
        <#code#>
    }
}
