//
//  ErrorIndicator.swift
//  Nodal
//
//  Created by Devin Lehmacher on 3/26/23.
//

import SwiftUI

/// TODO: it would be good to add a more user friendly indicator before productionizing this
/// that would probably take the form of a (trailing) closure argument that turns the error into
/// a more generic error we can present to the user
struct ErrorIndicator: View {
    let error: Error

    init(for error: Error) {
        self.error = error
    }

    @State private var presentingAlert = false

    var body: some View {
        Button {
            presentingAlert = true
        } label: {
            Label("Show Error", systemImage: "exclamationmark.circle.fill")
        }
        .tint(.red)
        .alert("An Error Occurred", isPresented: $presentingAlert) {
            Button("Ok") {
                presentingAlert = false
            }
        } message: {
            Text(error.localizedDescription)
        }
    }
}

// periphery:ignore
private struct TestError: LocalizedError {
    let code = "I am a error"
    let reason = "This is a test"
}

struct ErrorIndicator_Previews: PreviewProvider {
    static var previews: some View {
        ErrorIndicator(for: TestError())
    }
}
