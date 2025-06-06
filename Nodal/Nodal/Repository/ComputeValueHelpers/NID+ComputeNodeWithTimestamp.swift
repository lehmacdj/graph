//
//  NID+ComputeNodeWithTimestamp.swift
//  Nodal
//
//  Created by Devin Lehmacher on 12/15/24.
//

import Algorithms
import Foundation

struct TimestampAugmentation {
    let timestamp: Date?
}

extension NID {
    private func parseAndFetch(
        node: Node<NoAugmentation>,
        timeBuilder: GraphTimestampBuilder,
        parse: (GraphTimestampBuilder, String) -> GraphTimestampBuilder?,
        dependencyManager: DependencyManager
    ) -> [(GraphTimestampBuilder, Node<NoAugmentation>)?] {
        node.incoming.flatMap { (transition, nids) in
            // Attempt to parse the transition to get a new builder
            guard let updatedBuilder = parse(timeBuilder, transition) else {
                return [(GraphTimestampBuilder, Node<NoAugmentation>)?]()
            }
            // For each subsequent NID, try fetching the node and pair it with the updated builder
            return nids
                .map { try? dependencyManager.fetch(nid: $0, dataNeed: .dataNotNeeded) }
                .map { maybeNodeValue in
                    maybeNodeValue.map { (updatedBuilder, $0) }
                }
        }
    }


    func computeNodeWithTimestamp(dependencyManager: DependencyManager) throws(FetchDependencyError) -> Node<TimestampAugmentation> {
        let node = try dependencyManager.fetch(nid: self, dataNeed: .dataNotNeeded)

        let candidateDays: [(GraphTimestampBuilder, Node<NoAugmentation>)] = try parseAndFetch(
                node: node,
                timeBuilder: GraphTimestampBuilder(),
                parse: { builder, transition in builder.parseTime(from: transition) },
                dependencyManager: dependencyManager
            )
            .compactedSameSize(elseThrow: FetchDependencyError.missingDependencies)

        let candidateMonths: [(GraphTimestampBuilder, Node<NoAugmentation>)] = try candidateDays
            .flatMap { (timeBuilder, node) in
                parseAndFetch(
                    node: node,
                    timeBuilder: timeBuilder,
                    parse: { builder, transition in builder.parseDay(from: transition) },
                    dependencyManager: dependencyManager
                )
            }
            .compactedSameSize(elseThrow: FetchDependencyError.missingDependencies)

        let candidateYears: [(GraphTimestampBuilder, Node<NoAugmentation>)] = try candidateMonths
            .flatMap { (timeBuilder, node) in
                parseAndFetch(
                    node: node,
                    timeBuilder: timeBuilder,
                    parse: { builder, transition in builder.parseMonth(from: transition) },
                    dependencyManager: dependencyManager
                )
            }
            .compactedSameSize(elseThrow: FetchDependencyError.missingDependencies)

        let candidateImportDates: [(GraphTimestampBuilder, Node<NoAugmentation>)] = try candidateYears
            .flatMap { (timeBuilder, node) in
                parseAndFetch(
                    node: node,
                    timeBuilder: timeBuilder,
                    parse: { builder, transition in builder.parseYear(from: transition) },
                    dependencyManager: dependencyManager
                )
            }
            .compactedSameSize(elseThrow: FetchDependencyError.missingDependencies)

        let completeDates: [Date] = candidateImportDates
            .filter { $0.1.id == NID.importDates }
            .compactMap { $0.0.build() }

        return node.withAugmentation(TimestampAugmentation(timestamp: completeDates.max()))
    }
}
