// Geometry utilities for rectangle operations
window.ImageExtension = window.ImageExtension || {};
window.ImageExtension.geometry = {
  /**
   * Calculate the intersection of two rectangles
   * @param {DOMRect} rect1 - First rectangle
   * @param {DOMRect} rect2 - Second rectangle
   * @returns {Object} Intersection rectangle with left, top, right, bottom, width, height
   */
  intersectRects: function(rect1, rect2) {
    const left = Math.max(rect1.left, rect2.left);
    const top = Math.max(rect1.top, rect2.top);
    const right = Math.min(rect1.right, rect2.right);
    const bottom = Math.min(rect1.bottom, rect2.bottom);
    const width = Math.max(0, right - left);
    const height = Math.max(0, bottom - top);
    
    return {
      left,
      top,
      right,
      bottom,
      width,
      height
    };
  }
};