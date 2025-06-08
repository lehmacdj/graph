// Image detection and finding utilities
window.ImageExtension = window.ImageExtension || {};
window.ImageExtension.imageDetection = {
  /**
   * Get the URL from an image element (IMG or SVG image)
   * @param {Element} element - Image element
   * @returns {string|null} Image URL or null
   */
  getImageURL: function(element) {
    if (element.tagName === "IMG") {
      return element.src || element.currentSrc;
    } else if (element.tagName === "image") {
      return element.href.baseVal || element.getAttribute("href");
    }
    return null;
  },

  /**
   * Get bounding rectangle for an element, with SVG clipping for image elements
   * @param {Element} element - Element to get bounds for
   * @returns {DOMRect} Bounding rectangle
   */
  getBoundingRect: function(element) {
    let rect = element.getBoundingClientRect();
    if (element.tagName === "image") {
      // intersect the rect with the SVG element's bounding box
      const svgParent = window.ImageExtension.domUtils.outermostSVGAncestor(element);
      if (svgParent) {
        const svgRect = svgParent.getBoundingClientRect();
        const clippedRect = window.ImageExtension.geometry.intersectRects(rect, svgRect);
        
        // Only return clipped rect if it has valid dimensions
        if (clippedRect.width > 0 && clippedRect.height > 0) {
          rect = clippedRect;
        } else {
          rect = svgRect; // Fallback to SVG bounding box if no intersection
        }
      }
    }
    return rect;
  },

  /**
   * Find all images in a container, optionally filtered by a point
   * @param {Element} container - Container to search in
   * @param {Object|null} point - Point with x,y coordinates to filter by
   * @returns {Array} Array of image data objects
   */
  findImagesInContainer: function(container, point = null) {
    let images = Array.from(container.querySelectorAll("img, image")).map(
      (img) => ({
        element: img,
        url: this.getImageURL(img),
        rect: this.getBoundingRect(img),
      })
    );

    images = images.filter((img) => img.url && window.ImageExtension.domUtils.isElementVisible(img.element));

    if (point) {
      images = images.filter((img) => {
        return (
          img.rect.left <= point.x &&
          img.rect.right >= point.x &&
          img.rect.top <= point.y &&
          img.rect.bottom >= point.y
        );
      });
    }

    return this.removeDuplicateUrls(images);
  },

  /**
   * Remove duplicate images based on URL
   * @param {Array} images - Array of image data objects
   * @returns {Array} Array with duplicates removed, sorted by z-index
   */
  removeDuplicateUrls: function(images) {
    const seenUrls = new Set();
    const uniqueImages = [];

    for (const img of images) {
      if (!seenUrls.has(img.url)) {
        seenUrls.add(img.url);
        uniqueImages.push(img);
      }
    }

    return uniqueImages.sort((a, b) => {
      const aZIndex = parseInt(window.getComputedStyle(a.element).zIndex) || 0;
      const bZIndex = parseInt(window.getComputedStyle(b.element).zIndex) || 0;
      return bZIndex - aZIndex;
    });
  },

  /**
   * Find images at a specific point
   * @param {number} x - X coordinate
   * @param {number} y - Y coordinate
   * @returns {Array} Array of image data objects at the point
   */
  findImagesAtPoint: function(x, y) {
    const result = this.findImagesInContainer(document, { x, y });
    return result;
  },

  /**
   * Find common ancestor and all images within it
   * @param {Array} imagesAtPoint - Array of image data objects
   * @returns {Object} Object with ancestor element and all images in ancestor
   */
  findAncestorAndImages: function(imagesAtPoint) {
    const ancestor = window.ImageExtension.domUtils.findCommonAncestor(
      imagesAtPoint.map((img) => img.element)
    );
    const allImagesInAncestor = this.findImagesInContainer(ancestor);
    return { ancestor, images: allImagesInAncestor };
  }
};