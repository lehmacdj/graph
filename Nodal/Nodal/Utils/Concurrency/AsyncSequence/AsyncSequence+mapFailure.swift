//
//  AsyncSequence+mapFailure.swift
//  Nodal
//
//  Created by Devin Lehmacher on 1/31/25.
//

struct ErrorMappingAsyncSequence<Base: AsyncSequence, TransformedFailure: Error>: AsyncSequence {
    let base: Base
    let transform: (Base.Failure) -> TransformedFailure

    func makeAsyncIterator() -> AsyncIterator {
        AsyncIterator(base: base.makeAsyncIterator(), transform: transform)
    }

    struct AsyncIterator: AsyncIteratorProtocol {
        private var base: Base.AsyncIterator
        private let transform: (Base.Failure) -> TransformedFailure

        init(base: Base.AsyncIterator, transform: @escaping (Base.Failure) -> TransformedFailure) {
            self.base = base
            self.transform = transform
        }

        mutating func next() async throws -> Base.Element? {
            try await next(isolation: #isolation)
        }

        mutating func next(isolation: isolated (any Actor)? = #isolation) async throws(TransformedFailure) -> Base.Element? {
            do {
                return try await base.next(isolation: isolation)
            } catch {
                throw transform(error)
            }
        }
    }
}

extension AsyncSequence {
    func mapFailure<TransformedFailure: Error>(
        _ transform: @escaping (Failure) -> TransformedFailure
    ) -> ErrorMappingAsyncSequence<Self, TransformedFailure> {
        .init(base: self, transform: transform)
    }
}
