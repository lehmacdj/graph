//
//  ZoomableScrollView.swift
//  view
//
// https://stackoverflow.com/questions/60637521/how-to-pinch-and-scroll-an-image-in-swiftui/
//

import SwiftUI

import SwiftUI
import Combine

class CenteringScrollView: UIScrollView {
    func centerContent() {
        assert(subviews.count == 1)
        // not clear why view.center.{x,y} = bounds.mid{X,Y} doesn't work -- maybe transform?
        subviews[0].frame.origin.x = max(0, bounds.width - subviews[0].frame.width) / 2
        subviews[0].frame.origin.y = max(0, bounds.height - subviews[0].frame.height) / 2
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        centerContent()
    }
}

struct ZoomableScrollView<Content: View>: View {
    let content: Content
    init(@ViewBuilder content: () -> Content) {
        self.content = content()
    }

    @State var doubleTap = PassthroughSubject<Void, Never>()

    var body: some View {
        ZoomableScrollViewImpl(content: content, doubleTap: doubleTap.eraseToAnyPublisher())
        /// The double tap gesture is a modifier on a SwiftUI wrapper view, rather than just putting a UIGestureRecognizer on the wrapped view,
        /// because SwiftUI and UIKit gesture recognizers don't work together correctly correctly for failure and other interactions.
            .onTapGesture(count: 2) {
                doubleTap.send()
            }
    }
}

fileprivate struct ZoomableScrollViewImpl<Content: View>: UIViewControllerRepresentable {
    let content: Content
    let doubleTap: AnyPublisher<Void, Never>

    func makeUIViewController(context: Context) -> ViewController {
        return ViewController(content: content, doubleTap: doubleTap)
    }

    func updateUIViewController(_ viewController: ViewController, context: Context) {
        viewController.update(content: self.content, doubleTap: doubleTap)
    }

    // MARK: - ViewController
    class ViewController: UIViewController, UIScrollViewDelegate {
        private let hostingController: UIHostingController<Content>
        let scrollView = CenteringScrollView()

        var doubleTapCancellable: Cancellable?
        var updateConstraintsCancellable: Cancellable?

        private var hostedView: UIView { hostingController.view! }

        private var contentSizeConstraints: [NSLayoutConstraint] = [] {
            willSet { NSLayoutConstraint.deactivate(contentSizeConstraints) }
            didSet { NSLayoutConstraint.activate(contentSizeConstraints) }
        }

        @available(*, unavailable)
        required init?(coder: NSCoder) { fatalError() }

        init(content: Content, doubleTap: AnyPublisher<Void, Never>) {
            self.hostingController = UIHostingController(rootView: content)
            super.init(nibName: nil, bundle: nil)
            self.view = scrollView

            scrollView.delegate = self  // for viewForZooming(in:)
            scrollView.maximumZoomScale = 10
            scrollView.minimumZoomScale = 1
            scrollView.bouncesZoom = true
            scrollView.showsHorizontalScrollIndicator = false
            scrollView.showsVerticalScrollIndicator = false
            scrollView.clipsToBounds = false

            let hostedView = hostingController.view!
            hostedView.translatesAutoresizingMaskIntoConstraints = false
            scrollView.addSubview(hostedView)
            NSLayoutConstraint.activate([
                hostedView.leadingAnchor.constraint(equalTo: scrollView.contentLayoutGuide.leadingAnchor),
                hostedView.trailingAnchor.constraint(equalTo: scrollView.contentLayoutGuide.trailingAnchor),
                hostedView.topAnchor.constraint(equalTo: scrollView.contentLayoutGuide.topAnchor),
                hostedView.bottomAnchor.constraint(equalTo: scrollView.contentLayoutGuide.bottomAnchor),
            ])

            updateConstraintsCancellable = scrollView.publisher(for: \.bounds).map(\.size).removeDuplicates()
                .sink { [unowned self] size in
                    view.setNeedsUpdateConstraints()
                }
            doubleTapCancellable = doubleTap.sink { [unowned self] in handleDoubleTap() }
        }

        func update(content: Content, doubleTap: AnyPublisher<Void, Never>) {
            hostingController.rootView = content
            scrollView.setNeedsUpdateConstraints()
            doubleTapCancellable = doubleTap.sink { [unowned self] in handleDoubleTap() }
        }

        func handleDoubleTap() {
            scrollView.setZoomScale(scrollView.zoomScale >= 1 ? scrollView.minimumZoomScale : 1, animated: true)
        }

        override func updateViewConstraints() {
            super.updateViewConstraints()
            let hostedContentSize = hostingController.sizeThatFits(in: view.bounds.size)
            contentSizeConstraints = [
                hostedView.widthAnchor.constraint(equalToConstant: hostedContentSize.width),
                hostedView.heightAnchor.constraint(equalToConstant: hostedContentSize.height),
            ]
        }

        override func viewDidAppear(_ animated: Bool) {
            scrollView.zoom(to: hostedView.bounds, animated: false)
        }

        override func viewDidLayoutSubviews() {
            super.viewDidLayoutSubviews()

            let hostedContentSize = hostingController.sizeThatFits(in: view.bounds.size)
            scrollView.minimumZoomScale = min(
                scrollView.bounds.width / hostedContentSize.width,
                scrollView.bounds.height / hostedContentSize.height)
        }

        func scrollViewDidZoom(_ scrollView: UIScrollView) {
            // For some reason this is needed in both didZoom and layoutSubviews, thanks to https://medium.com/@ssamadgh/designing-apps-with-scroll-views-part-i-8a7a44a5adf7
            // Sometimes this seems to work (view animates size and position simultaneously from current position to center) and sometimes it does not (position snaps to center immediately, size change animates)
            self.scrollView.centerContent()
        }

        override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
            coordinator.animate(alongsideTransition: nil) { [self] context in
                scrollView.zoom(to: hostedView.bounds, animated: false)
            }
        }

        func viewForZooming(in scrollView: UIScrollView) -> UIView? {
            return hostedView
        }
    }
}

