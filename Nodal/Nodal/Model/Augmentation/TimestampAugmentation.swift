//
//  TimestampAugmentation.swift
//  Nodal
//
//  Created by Devin Lehmacher on 12/15/24.
//

import Algorithms
import Foundation

extension NID {
    private func parseAndFetch(
        node: NodeValue<AugmentationDataValue>,
        timeBuilder: GraphTimestampBuilder,
        parse: (GraphTimestampBuilder, String) -> GraphTimestampBuilder?,
        fetchDependency: FetchDependencyClosure
    ) -> [(GraphTimestampBuilder, NodeValue<AugmentationDataValue>)?] {
        node.incoming.flatMap { (transition, nids) in
            // Attempt to parse the transition to get a new builder
            guard let updatedBuilder = parse(timeBuilder, transition) else {
                return [(GraphTimestampBuilder, NodeValue<AugmentationDataValue>)?]()
            }
            // For each subsequent NID, try fetching the node and pair it with the updated builder
            return nids
                .map { try? fetchDependency($0, .dataNotNeeded) }
                .map { maybeNodeValue in
                    maybeNodeValue.map { (updatedBuilder, $0) }
                }
        }
    }


    func computeNodeWithTimestamp(fetchDependency: FetchDependencyClosure) throws -> NodeValue<Date?> {
        let node = try fetchDependency(self, .dataNotNeeded)

        let candidateDays: [(GraphTimestampBuilder, NodeValue<AugmentationDataValue>)] = try parseAndFetch(
                node: node,
                timeBuilder: GraphTimestampBuilder(),
                parse: { builder, transition in builder.parseTime(from: transition) },
                fetchDependency: fetchDependency
            )
            .compactedSameSize(elseThrow: FetchDependencyError.missingDependencies)

        let candidateMonths: [(GraphTimestampBuilder, NodeValue<AugmentationDataValue>)] = try candidateDays
            .flatMap { (timeBuilder, node) in
                parseAndFetch(
                    node: node,
                    timeBuilder: timeBuilder,
                    parse: { builder, transition in builder.parseDay(from: transition) },
                    fetchDependency: fetchDependency
                )
            }
            .compactedSameSize(elseThrow: FetchDependencyError.missingDependencies)

        let candidateYears: [(GraphTimestampBuilder, NodeValue<AugmentationDataValue>)] = try candidateMonths
            .flatMap { (timeBuilder, node) in
                parseAndFetch(
                    node: node,
                    timeBuilder: timeBuilder,
                    parse: { builder, transition in builder.parseMonth(from: transition) },
                    fetchDependency: fetchDependency
                )
            }
            .compactedSameSize(elseThrow: FetchDependencyError.missingDependencies)

        let candidateImportDates: [(GraphTimestampBuilder, NodeValue<AugmentationDataValue>)] = try candidateYears
            .flatMap { (timeBuilder, node) in
                parseAndFetch(
                    node: node,
                    timeBuilder: timeBuilder,
                    parse: { builder, transition in builder.parseYear(from: transition) },
                    fetchDependency: fetchDependency
                )
            }
            .compactedSameSize(elseThrow: FetchDependencyError.missingDependencies)

        let completeDates: [Date] = candidateImportDates
            .filter { $0.1.id == NID.importDates }
            .compactMap { $0.0.build() }

        return node.withAugmentation(completeDates.max())
    }
}
