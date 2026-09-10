//
//  TransitionVMTests.swift
//  NodalTests
//
//  Created by Devin Lehmacher on 9/10/26.
//

@testable import Nodal
import Foundation
import Testing

/// Poll `condition` until it holds, failing the test if it never does.
@MainActor
private func waitUntil(
    _ description: String,
    timeout: Duration = .seconds(2),
    _ condition: () async -> Bool
) async {
    let deadline = ContinuousClock.now + timeout
    while ContinuousClock.now < deadline {
        if await condition() { return }
        try? await Task.sleep(for: .milliseconds(5))
    }
    Issue.record("timed out waiting for \(description)")
}

@MainActor
private func makeVM(
    _ repository: MockGraphRepository
) -> GraphRepositoryTransitionVM {
    GraphRepositoryTransitionVM(
        graphRepository: repository,
        sourceNid: NID(fake: 1),
        transition: NodeTransition(transition: "link", nid: NID(fake: 2)),
        timestamp: .loaded(nil),
        configuredForSection: .other
    )
}

private var isCloudFile: (Loading<ThumbnailValue>) -> Bool {
    { if case .loaded(.cloudFile) = $0 { true } else { false } }
}

private var isLoadingThumbnail: (Loading<ThumbnailValue>) -> Bool {
    { if case .loaded(.thumbnail(.loading)) = $0 { true } else { false } }
}

/// Recreating a cell must not leave its view model unsubscribed.
///
/// SwiftUI starts a replacement row's `.task` before the old row's `.task` has
/// finished being cancelled, so for a moment the view model has two overlapping
/// subscribers. The replacement used to see the previous subscription still in
/// place and bail out, and then the previous one went away: the cell was left
/// with no update loop at all, so tapping it did nothing until scrolling
/// recreated the row.
@MainActor
@Test func overlappingSubscriptionsLeaveTheCellUsable() async {
    let url = URL(filePath: "/remote/file")
    let repository = MockGraphRepository(localData: .remoteDataExists(url))
    let vm = makeVM(repository)

    let oldRowTask = Task { await vm.subscribe() }
    await waitUntil("thumbnail to be recognized as a cloud file") {
        isCloudFile(vm.thumbnail)
    }

    // the row gets recreated
    let newRowTask = Task { await vm.subscribe() }
    defer { newRowTask.cancel() }
    try? await Task.sleep(for: .milliseconds(50))
    oldRowTask.cancel()
    try? await Task.sleep(for: .milliseconds(50))

    // the tap
    vm.fetchThumbnail()

    #expect(isLoadingThumbnail(vm.thumbnail))
    await waitUntil("the download to be requested") {
        let needs = await repository.requestedDataNeeds
        return needs.contains(.needDataEvenIfRemote)
    }
}

/// Tapping a not-yet-downloaded cell shows its spinner right away.
@MainActor
@Test func tappingCloudFileShowsLoadingAndRequestsDownload() async {
    let url = URL(filePath: "/remote/file")
    let repository = MockGraphRepository(localData: .remoteDataExists(url))
    let vm = makeVM(repository)

    let subscription = Task { await vm.subscribe() }
    defer { subscription.cancel() }
    await waitUntil("thumbnail to be recognized as a cloud file") {
        isCloudFile(vm.thumbnail)
    }

    vm.fetchThumbnail()

    // synchronously, without waiting for the download to even start
    #expect(isLoadingThumbnail(vm.thumbnail))

    await waitUntil("the download to be requested") {
        let needs = await repository.requestedDataNeeds
        return needs.contains(.needDataEvenIfRemote)
    }
}

/// Erasing the same view model twice has to produce the same `ForEach` identity,
/// otherwise every graph update tears down and rebuilds every row of the list.
@MainActor
@Test func erasingATransitionVMPreservesItsIdentity() {
    let repository = MockGraphRepository()
    let vm = makeVM(repository)

    let other = makeVM(repository)

    #expect(vm.eraseToAnyTransitionVM().id == vm.eraseToAnyTransitionVM().id)
    #expect(other.eraseToAnyTransitionVM().id != vm.eraseToAnyTransitionVM().id)
}
