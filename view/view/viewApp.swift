//
//  viewApp.swift
//  view
//
//  Created by Devin Lehmacher on 10/10/21.
//

import SwiftUI

@main
struct viewApp: App {
    var body: some Scene {
        DocumentGroup(viewing: GraphDocument.self) { graphDir in
            ContentView(document: graphDir.$document)
        }
    }
}
