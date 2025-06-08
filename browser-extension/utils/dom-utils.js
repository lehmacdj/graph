// DOM traversal and visibility utilities
window.ImageExtension = window.ImageExtension || {};
window.ImageExtension.domUtils = {
  /**
   * Find the common ancestor of multiple elements
   * @param {Array<Element>} elements - Array of DOM elements
   * @returns {Element|null} Common ancestor element
   */
  findCommonAncestor: function(elements) {
    if (elements.length === 0) return null;
    if (elements.length === 1) return elements[0];

    let commonAncestor = elements[0];
    for (let i = 1; i < elements.length; i++) {
      commonAncestor = this.findClosestCommonAncestor(commonAncestor, elements[i]);
    }
    return commonAncestor;
  },

  /**
   * Find the closest common ancestor of two elements
   * @param {Element} element1 - First element
   * @param {Element} element2 - Second element
   * @returns {Element} Closest common ancestor
   */
  findClosestCommonAncestor: function(element1, element2) {
    const ancestors1 = [];
    let current = element1;
    while (current) {
      ancestors1.push(current);
      current = current.parentElement;
    }

    current = element2;
    while (current) {
      if (ancestors1.includes(current)) {
        return current;
      }
      current = current.parentElement;
    }
    return document.documentElement;
  },

  /**
   * Check if an element is visible (not hidden by CSS)
   * @param {Element} element - Element to check
   * @returns {boolean} True if element is visible
   */
  isElementVisible: function(element) {
    const style = window.getComputedStyle(element);
    return (
      style.visibility !== "hidden" &&
      style.display !== "none" &&
      (element.parentElement === null ||
        this.isElementVisible(element.parentElement))
    );
  },

  /**
   * Find the outermost SVG ancestor of an element
   * If it is possible for HTML to be nested inside SVG this might not be accurate
   * @param {Element} element - Element to find SVG ancestor for
   * @returns {SVGElement|null} Outermost SVG ancestor or null
   */
  outermostSVGAncestor: function(element) {
    if (!element || !(element instanceof SVGElement)) {
      return null;
    }
    let result = element;
    let next = element.parentElement;
    while (next && next instanceof SVGElement) {
      result = next;
      next = next.parentElement;
    }
    return result;
  }
};