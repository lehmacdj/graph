//
//  GraphRepository.swift
//  Nodal
//
//  Created by Devin Lehmacher on 11/2/24.
//

protocol GraphRepository: Actor {
    associatedtype NodeHandle: Nodal.NodeHandle

    func createNewNode() async throws -> NID
    func getNode(withId id: NID) async throws -> NodeHandle
    func deleteNode(withId id: NID) async throws

    func insertEdge(from: NID, to: NID) async throws
    func deleteEdge(from: NID, to: NID) async throws
}
