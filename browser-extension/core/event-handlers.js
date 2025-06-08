// Event handling logic and state coordination
window.ImageExtension = window.ImageExtension || {};
window.ImageExtension.eventHandlers = {
  /**
   * Handle mouse move events
   * @param {MouseEvent} e - Mouse event
   * @param {Object} state - Current extension state
   */
  handleMouseMove: function(e, state) {
    const x = e.clientX;
    const y = e.clientY;

    // Don't dismiss menu if hovering over it or in transition zone
    if (
      state.currentMenu &&
      (state.currentMenu.contains(e.target) || 
       window.ImageExtension.menu.isInMenuTransitionZone(x, y, state.currentMenu, state.currentAncestor))
    ) {
      return;
    }

    const imagesAtPoint = window.ImageExtension.imageDetection.findImagesAtPoint(x, y);

    if (imagesAtPoint.length === 0) {
      // Only remove menu if we're not in the transition zone
      if (!window.ImageExtension.menu.isInMenuTransitionZone(x, y, state.currentMenu, state.currentAncestor)) {
        this.removeMenu(state);
      }
      return;
    }

    // Find ancestor and validate it
    const { ancestor, images: allImagesInAncestor } =
      window.ImageExtension.imageDetection.findAncestorAndImages(imagesAtPoint);

    // Check if we're still within the same ancestor element with same images
    if (
      state.currentAncestor === ancestor &&
      state.currentImages.length === allImagesInAncestor.length &&
      state.currentImages.every(
        (img, i) => img.element === allImagesInAncestor[i].element
      )
    ) {
      // Validate we're still in a reasonable area
      const ancestorRect = state.currentAncestor.getBoundingClientRect();
      if (
        x >= ancestorRect.left &&
        x <= ancestorRect.right &&
        y >= ancestorRect.top &&
        y <= ancestorRect.bottom
      ) {
        return;
      }
    }

    this.updateMenuForImages(ancestor, allImagesInAncestor, imagesAtPoint, state);
  },

  /**
   * Update menu for a new set of images
   * @param {Element} ancestor - Ancestor element
   * @param {Array} allImagesInAncestor - All images in ancestor
   * @param {Array} imagesAtPoint - Images at current point
   * @param {Object} state - Current extension state
   */
  updateMenuForImages: function(ancestor, allImagesInAncestor, imagesAtPoint, state) {
    this.removeMenu(state);

    state.currentImages = allImagesInAncestor;
    state.currentHoverElement = imagesAtPoint.length > 0 ? imagesAtPoint[0].element : null;
    state.currentAncestor = ancestor;

    // Use smart positioning
    const { x: menuX, y: menuY } = window.ImageExtension.menu.calculateMenuPosition(
      ancestor,
      allImagesInAncestor
    );

    state.currentMenu = window.ImageExtension.menu.createImageMenu(allImagesInAncestor, menuX, menuY);
    document.body.appendChild(state.currentMenu);
  },

  /**
   * Handle mouse leave events
   * @param {MouseEvent} e - Mouse event
   * @param {Object} state - Current extension state
   */
  handleMouseLeave: function(e, state) {
    const relatedTarget = e.relatedTarget;

    if (
      state.currentMenu &&
      (relatedTarget === state.currentMenu || state.currentMenu.contains(relatedTarget))
    ) {
      return;
    }

    if (
      state.currentAncestor &&
      (relatedTarget === state.currentAncestor ||
        state.currentAncestor.contains(relatedTarget))
    ) {
      return;
    }

    setTimeout(() => {
      if (state.currentMenu && !state.currentMenu.matches(":hover")) {
        this.removeMenu(state);
      }
    }, 100);
  },

  /**
   * Remove current menu and reset state
   * @param {Object} state - Current extension state
   */
  removeMenu: function(state) {
    if (state.currentMenu) {
      state.currentMenu.remove();
      state.currentMenu = null;
    }

    // Force reset all state
    state.currentImages = [];
    state.currentHoverElement = null;
    state.currentAncestor = null;
  },

  /**
   * Handle scroll events - reposition menu
   * @param {Object} state - Current extension state
   */
  handleScroll: function(state) {
    if (state.currentMenu && state.currentAncestor && state.currentImages.length > 0) {
      const { x: menuX, y: menuY } = window.ImageExtension.menu.calculateMenuPosition(
        state.currentAncestor,
        state.currentImages
      );
      state.currentMenu.style.left = menuX + "px";
      state.currentMenu.style.top = menuY + "px";
    }
  },

  /**
   * Handle resize events - reposition menu
   * @param {Object} state - Current extension state
   */
  handleResize: function(state) {
    if (state.currentMenu && state.currentAncestor && state.currentImages.length > 0) {
      const { x: menuX, y: menuY } = window.ImageExtension.menu.calculateMenuPosition(
        state.currentAncestor,
        state.currentImages
      );
      state.currentMenu.style.left = menuX + "px";
      state.currentMenu.style.top = menuY + "px";
    }
  }
};