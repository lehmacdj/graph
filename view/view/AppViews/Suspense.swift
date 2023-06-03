//
//  Suspense.swift
//  view
//
//  Created by Devin Lehmacher on 4/2/23.
//

import SwiftUI

struct Suspense<T, Content: View, Placeholder: View, ErrorDisplay: View>: View {
    let result: Loading<T>
    let content: (T) -> Content
    let placeholder: () -> Placeholder
    let errorDisplay: (Error) -> ErrorDisplay

    init(
        _ result: Loading<T>,
        @ViewBuilder content: @escaping (T) -> Content,
        @ViewBuilder placeholder: @escaping () -> Placeholder = { ProgressView() },
        @ViewBuilder errorDisplay: @escaping (Error) -> ErrorDisplay = {
            ErrorIndicator(for: $0)
        }
    ) {
        self.result = result
        self.content = content
        self.placeholder = placeholder
        self.errorDisplay = errorDisplay
    }

    var body: some View {
        switch result {
        case .idle:
            placeholder()
        case .loading:
            placeholder()
        case .loaded(let t):
            content(t)
        case .failed(let error):
            errorDisplay(error)
        }
    }
}

fileprivate struct SampleError: Error {}

struct Suspense_Previews: PreviewProvider {
    static var previews: some View {
        let loadingStates: [Loading<Int>] = [
            .idle,
            .loading,
            .loaded(91),
            .failed(SampleError())
        ]
        VStack {
            ForEach(loadingStates) { state in
                Suspense(state) { value in
                    Text("\(value)")
                }.padding()
            }
        }
    }
}
