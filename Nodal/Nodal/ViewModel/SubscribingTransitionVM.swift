//
//  SubscribingTransitionVM.swift
//  Nodal
//
//  Created by Devin Lehmacher on 8/7/23.
//

import Foundation
import SwiftUI
import Combine
import Observation

@Observable class SubscribingTransitionVM<N: Node>: TransitionVM {
    let direction: Direction
    let destinationNid: NID
    var transition: String
    var thumbnail: Loading<ThumbnailValue> {
        switch internalState {
        case .idle:
            return .idle
        case .loadingActive(thumbnail: nil):
            return .loading
        case .loadingActive(thumbnail: .some(let state)):
            // even though we're internally loading this indicates that we were fully loaded before & thus we return the loaded information we already had
            return .loaded(state)
        case .loadingInactive:
            return .loading
        case .loadedActive(let state, _, _):
            return .loaded(state)
        case .loadedInactive(let state, _):
            return .loaded(state)
        case .failed(let error):
            return .failed(error)
        }
    }
    var timestamp: Loading<Date?> {
        switch internalState {
        case .idle:
            return .idle
        case .loadingActive:
            return .loading
        case .loadingInactive:
            return .loading
        case .loadedActive(_, let timestamp, _):
            return .loaded(timestamp)
        case .loadedInactive(_, let timestamp):
            return .loaded(timestamp)
        case .failed:
            return .idle
        }
    }
    var isFavorite: Bool
    var isWorse: Bool

    // this caching is probably unnecessary, but it makes debugging some stuff a little less noisy and shouldn't be harmful
    @ObservationIgnored
    private weak var _destination: AnyNodeVM?
    var destination: AnyNodeVM {
        if let _destination {
            return _destination
        } else {
            let destination = SubscribingNodeVM(for: destinationNid, in: manager).eraseToAnyNodeVM()
            _destination = destination
            return destination
        }
    }

    enum InternalState: Equatable {
        static func == (lhs: SubscribingTransitionVM<N>.InternalState, rhs: SubscribingTransitionVM<N>.InternalState) -> Bool {
            switch (lhs, rhs) {
            case (.idle, .idle):
                true
            case (.loadingActive(let lhs), .loadingActive(let rhs)):
                lhs == rhs
            case (.loadingInactive, .loadingInactive):
                true
            case (.loadedActive(let lhsThumbnail, let lhsDestinationTimestamp, let lhsDestinationNode),
                  .loadedActive(let rhsThumbnail, let rhsDestinationTimestamp, let rhsDestinationNode)):
                lhsThumbnail == rhsThumbnail && lhsDestinationTimestamp == rhsDestinationTimestamp
                    && lhsDestinationNode.nid == rhsDestinationNode.nid
            case (.loadedInactive(let lhsThumbnail, let lhsDestinationTimestamp),
                  .loadedInactive(let rhsThumbnail, let rhsDestinationTimestamp)):
                lhsThumbnail == rhsThumbnail && lhsDestinationTimestamp == rhsDestinationTimestamp
            case (.failed(let lhsError), .failed(let rhsError)):
                lhsError.localizedDescription == rhsError.localizedDescription
            default:
                false
            }
        }

        case idle
        /// `loadingActive` captures a few substates all of which are valid
        /// * transition could be non-nil if we were formerly loaded and thus have a value
        /// * the node is populated during the loading process and is expected to exist for the final stage (initial data fetch) of the loading process
        case loadingActive(thumbnail: ThumbnailValue?)
        case loadingInactive
        case loadedActive(thumbnail: ThumbnailValue, destinationTimestamp: Date?, destinationNode: N)
        case loadedInactive(thumbnail: ThumbnailValue, destinationTimestamp: Date?)
        case failed(error: Error)

        var node: N? {
            switch self {
            case .idle, .loadingInactive, .loadingActive, .loadedInactive, .failed:
                return nil
            case .loadedActive(_, _, let node):
                return node
            }
        }
    }

    private var internalState: InternalState = .idle {
        didSet {
            _internalState = internalState
        }
    }
    @ObservationIgnored
    @Published
    private var publishedInternalState: InternalState = .idle

    private let manager: GraphManager<N>

    /// link to parent node, should always be retained because the transition is only ever presented as a part of a node
    /// NOTE: this assumption will break if I implement navigation that allows you to traverse the graph other than through child links
    private weak var parent: SubscribingNodeVM<N>!

    private let source: N

    init(
        parent: SubscribingNodeVM<N>,
        source: N,
        transition: NodeTransition,
        direction: Direction,
        manager: GraphManager<N>,
        isFavorite: Bool,
        isWorse: Bool
    ) {
        self.parent = parent
        self.source = source
        self.transition = transition.transition
        self.direction = direction
        self.manager = manager
        self.destinationNid = transition.nid
        self.isFavorite = isFavorite
        self.isWorse = isWorse
    }

    struct DuplicateSubscription: LocalizedError, Codable {}

    private func beginUpdateState() async throws {
        switch internalState {
        case .idle:
            internalState = .loadingActive(thumbnail: nil)
        case .loadingInactive:
            internalState = .loadingActive(thumbnail: nil)
        case .loadedInactive(let thumbnail, _):
            internalState = .loadingActive(thumbnail: thumbnail)
        case .failed(_):
            // we don't want to start loading failed nodes so that the error message doesn't get cleared
            // we sleep for 1 second and then check the current state again, so that the VM can get unstuck if internalState gets set to something other than failed
            try await Task.sleep(for: .seconds(1))
            return
        case .loadingActive, .loadedActive:
            // Swift UI seems to call this multiple times so we need to be able to exit in this case
            throw DuplicateSubscription()
        }

        let destinationNode: N
        let destinationTimestamp: Date?
        do {
            destinationNode = try await manager[destinationNid]
            destinationTimestamp = await destinationNode.mostRecentTimestamp
        } catch {
            logError(error.localizedDescription)
            internalState = .failed(error: error)
            return
        }

        guard case .loadingActive(let thumbnail) = internalState else {
            logWarn("state changed, need to re-begin loading state")
            return
        }

        if let thumbnail {
            internalState = .loadedActive(
                thumbnail: thumbnail,
                destinationTimestamp: destinationTimestamp,
                destinationNode: destinationNode
            )
        } else {
            if destinationNode.dataURL != nil {
                if destinationNode.dataRequiresDownload {
                    internalState = .loadedActive(
                        thumbnail: .cloudFile,
                        destinationTimestamp: destinationTimestamp,
                        destinationNode: destinationNode
                    )
                } else {
                    internalState = .loadedActive(
                        thumbnail: .thumbnail(.loading),
                        destinationTimestamp: destinationTimestamp,
                        destinationNode: destinationNode
                    )
                }
            } else {
                internalState = .loadedActive(
                    thumbnail: .noThumbnail,
                    destinationTimestamp: destinationTimestamp,
                    destinationNode: destinationNode
                )
            }
        }


        try await updateStateLoop()
    }

    private func updateStateLoop() async throws {
        guard case .loadedActive(let thumbnail, let destinationTimestamp, let destinationNode) = internalState else { return }

        if case .thumbnail(.loading) = thumbnail {
            await fetchThumbnail()
        }

        for await change in await manager.dataChanges(for: destinationNid).values {
            switch change {
            case .added, .changed:
                if !destinationNode.dataRequiresDownload {
                    await fetchThumbnail()
                } else {
                    // if we are in an inconsistent state we have to re-run beginUpdateState
                    guard case .loadedActive = internalState else { return }
                    internalState = .loadedActive(
                        thumbnail: .cloudFile,
                        destinationTimestamp: destinationTimestamp,
                        destinationNode: destinationNode
                    )
                }
            case .removed:
                // if we are in an inconsistent state we have to re-run beginUpdateState
                guard case .loadedActive = internalState else { return }
                internalState = .loadedActive(
                    thumbnail: .noThumbnail,
                    destinationTimestamp: destinationTimestamp,
                    destinationNode: destinationNode
                )
            }
        }
    }

    func subscribe() async {
        do {
            while true {
                try await beginUpdateState()
                try Task.checkCancellation()
            }
        } catch is CancellationError {
            logDebug("cancelled transition \(transition) to \(destinationNid)")
        } catch is DuplicateSubscription {
            logInfo("\(parent.nid) \(transition) \(destinationNid) duplicate subscription, exiting")
            return
        } catch {
            logError("\(parent.nid) \(transition) \(destinationNid) unexpected error thrown \(error)")
            internalState = .failed(error: error)
        }

        switch internalState {
        case .loadingActive(thumbnail: nil):
            internalState = .loadingInactive
        case .loadingActive(let .some(thumbnail)):
            internalState = .loadedInactive(thumbnail: thumbnail, destinationTimestamp: nil)
        case .loadedActive(let thumbnail, let destinationTimestamp, _):
            internalState = .loadedInactive(thumbnail: thumbnail, destinationTimestamp: destinationTimestamp)
        case .idle, .loadingInactive, .loadedInactive, .failed:
            // nothing to do already in an okay state
            break
        }
    }

    func toggleFavorite() async {
        do {
            try await parent.toggleFavorite(child: destinationNid)
        } catch {
            logError("failed to toggle favorite: \(error)")
        }
        isFavorite.toggle()
    }

    func toggleWorse() async {
        do {
            try await parent.toggleWorse(child: destinationNid)
        } catch {
            logError("failed to toggle worse: \(error)")
        }
        isWorse.toggle()
    }

    func fetchThumbnail() async {
        guard case .loadedActive(_, let destinationTimestamp, let destinationNode) = internalState else {
            return
        }

        do {
            logDebug("starting to fetch thumbnail")
            let data = try await destinationNode.data.unwrapped("doesn't have data")
            if let image = UIImage(data: data) {
                guard case .loadedActive = internalState else {
                    logWarn("internalState of transition \(transition) to \(destinationNid) has changed")
                    return
                }
                internalState = .loadedActive(thumbnail: .thumbnail(.loaded(image)), destinationTimestamp: destinationTimestamp, destinationNode: destinationNode)
            } else {
                guard case .loadedActive = internalState else {
                    logWarn("internalState of transition \(transition) to \(destinationNid) has changed")
                    return
                }
                internalState = .loadedActive(thumbnail: .noThumbnail, destinationTimestamp: destinationTimestamp, destinationNode: destinationNode)
            }
            logDebug("successfully loaded thumbnail")
        } catch {
            guard case .loadedActive = internalState else {
                logWarn("internalState of transition \(transition) to \(destinationNid) has changed")
                return
            }
            logError("failed to load thumbnail: \(error)")
            internalState = .loadedActive(thumbnail: .thumbnail(.failed(error)), destinationTimestamp: destinationTimestamp, destinationNode: destinationNode)
        }
    }

    func updateTransitionName(to newName: String) async {
        await manager.removeLink(from: source.nid, to: destinationNid, via: transition)
        await manager.addLink(from: source.nid, to: destinationNid, via: newName)
    }

    func removeTransition() async {
        await manager.removeLink(from: source.nid, to: destinationNid, via: transition)
    }

    /// Publisher that publishes whenever the state of this VM changes
    func updatedPublisher() -> AnyPublisher<Void, Never> {
        $publishedInternalState
            .removeDuplicates()
            .map { _ in () }
            .eraseToAnyPublisher()
    }
}

extension SubscribingTransitionVM: Identifiable {
    var id: some Hashable {
        "\(transition)\(destinationNid)"
    }
}

extension SubscribingTransitionVM: CustomDebugStringConvertible {
    var debugDescription: String {
        "\(parent?.nid.description ?? "some parent") -> \(destinationNid) via \(transition)"
    }
}
