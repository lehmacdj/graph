//
//  ZoomableViewTests.swift
//  Nodal
//

@testable import Nodal
import SwiftUI
import Testing
import UIKit

/// An image that is mostly `edge` colored with a `center` colored square in the
/// middle, so that a snapshot can tell whether the whole image is visible.
@MainActor
private func markedImage(
    size: CGSize,
    edge: UIColor = .systemBlue,
    center: UIColor = .systemGreen
) -> UIImage {
    UIGraphicsImageRenderer(size: size).image { context in
        edge.setFill()
        context.fill(CGRect(origin: .zero, size: size))
        center.setFill()
        let marker = CGSize(width: size.width / 4, height: size.height / 4)
        context.fill(CGRect(
            x: (size.width - marker.width) / 2,
            y: (size.height - marker.height) / 2,
            width: marker.width,
            height: marker.height
        ))
    }
}

@MainActor
private func snapshot(of view: some View, size: CGSize) async throws -> UIImage {
    let host = UIHostingController(rootView: view)
    let window = UIWindow(frame: CGRect(origin: .zero, size: size))
    window.rootViewController = host
    window.makeKeyAndVisible()
    host.view.layoutIfNeeded()
    // Let SwiftUI deliver geometry and settle before capturing.
    try await Task.sleep(for: .milliseconds(500))
    return UIGraphicsImageRenderer(bounds: window.bounds).image { context in
        window.layer.render(in: context.cgContext)
    }
}

extension UIImage {
    fileprivate func color(atX x: Int, y: Int) -> (r: Int, g: Int, b: Int)? {
        guard let cgImage,
              let data = cgImage.dataProvider?.data,
              let bytes = CFDataGetBytePtr(data)
        else { return nil }
        let px = Int(CGFloat(x) * scale)
        let py = Int(CGFloat(y) * scale)
        let offset = py * cgImage.bytesPerRow + px * (cgImage.bitsPerPixel / 8)
        guard offset + 2 < CFDataGetLength(data) else { return nil }
        return (Int(bytes[offset]), Int(bytes[offset + 1]), Int(bytes[offset + 2]))
    }
}

/// A `ZoomableView` must zoom to fit as soon as it has measured its content and
/// viewport. Doing this in `onAppear` doesn't work: `onAppear` runs before any
/// geometry has been reported, so the zoom was silently skipped and content was
/// left at actual size, showing a single pixel-level crop of a large image.
@MainActor
@Test func zoomableViewZoomsToFitInitially() async throws {
    let viewport = CGSize(width: 390, height: 800)
    let image = markedImage(size: CGSize(width: 3000, height: 2000))
    let snapshot = try await snapshot(
        of: ZoomableView { Image(uiImage: image) }
            .frame(width: viewport.width, height: viewport.height),
        size: viewport
    )

    // Sanity check that the snapshot captured something at all, so that a
    // blank snapshot can't be mistaken for a zoom failure.
    let corner = try #require(snapshot.color(atX: 2, y: 2))
    let sampled = (0..<20).compactMap {
        snapshot.color(atX: Int(viewport.width) / 2, y: 20 + $0 * 38)
    }
    #expect(sampled.contains { $0 != corner }, "snapshot appears to be blank")

    let center = try #require(
        snapshot.color(atX: Int(viewport.width / 2), y: Int(viewport.height / 2))
    )
    // At fit scale the center marker of the image is visible in the middle of
    // the viewport. At actual size the viewport shows a crop of the image's
    // top left corner instead, which is the edge color.
    #expect(center.g > center.r && center.g > center.b,
            "expected the center of the fitted image to be visible, got \(center)")
}
