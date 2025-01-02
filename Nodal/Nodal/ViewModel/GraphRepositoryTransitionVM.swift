//
//  GraphRepositoryTransitionVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 12/15/24.
//

import Foundation

final class GraphRepositoryTransitionVM: TransitionVM {
    private let graphRepository: GraphRepository

    init(
        graphRepository: GraphRepository,
        transition: NodeTransition,
        configuredForSection section: GraphRepositoryNodeVM.NodeSection
    ) {
        self.direction = section.direction
        self.isFavorite = section == .favorites
        self.isWorse = section == .worse
        self.destinationNid = transition.nid
        self.transition = transition.transition
        self.graphRepository = graphRepository
    }

    let destinationNid: NID
    var direction: Direction
    var transition: String
    var isFavorite: Bool
    var isWorse: Bool

    lazy var destination: AnyNodeVM =
        GraphRepositoryNodeVM(nid: destinationNid, graphRepository: graphRepository)
            .eraseToAnyNodeVM()

    func subscribe() async {
        async let _ = subscribeThumbnail()
    }

    // TODO: once I support data in GraphRepository
    var thumbnail: Loading<ThumbnailValue> = .loaded(.noThumbnail)

    private func subscribeThumbnail() async {
    }

    var timestamp: Loading<Date?> = .idle
    var isTimestampSubscribed = false

    var requireThumbnail: Bool = false

    func fetchThumbnail() async {
        requireThumbnail = true
    }

    // MARK: unimplemented for now because I don't support mutation right now

    func toggleFavorite() async {}

    func toggleWorse() async {}

    func updateTransitionName(to newName: String) async {}

    func removeTransition() async {}

    // MARK: special interop for GraphRepositoryNodeVM

    func configureForSection(_ section: GraphRepositoryNodeVM.NodeSection) {
        isFavorite = section == .favorites
        isWorse = section == .worse
        direction = section.direction
    }
}
