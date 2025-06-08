// Menu creation, positioning and state management
window.ImageExtension = window.ImageExtension || {};
window.ImageExtension.menu = {
  /**
   * Calculate optimal menu position relative to ancestor element
   * @param {Element} ancestor - Ancestor element to position relative to
   * @param {Array} images - Array of image data objects
   * @returns {Object} Position object with x,y coordinates
   */
  calculateMenuPosition: function(ancestor, images) {
    const MENU_WIDTH = 220;
    const MENU_HEIGHT = images.length * 40 + 20; // Estimate based on button height
    const ancestorRect = ancestor.getBoundingClientRect();
    const viewportWidth = window.innerWidth;
    const viewportHeight = window.innerHeight;

    // Check if ancestor is large enough (3x menu size)
    const isLargeAncestor =
      ancestorRect.width > MENU_WIDTH * 3 &&
      ancestorRect.height > MENU_HEIGHT * 3;

    let menuX, menuY;

    if (isLargeAncestor) {
      // Place inside the ancestor at a corner
      const padding = 10;

      // Try top-right corner first
      menuX = Math.min(
        ancestorRect.right - MENU_WIDTH - padding,
        viewportWidth - MENU_WIDTH - padding
      );
      menuY = Math.max(ancestorRect.top + padding, padding);

      // If menu would be outside viewport, try other corners
      if (menuX < padding) {
        menuX = Math.max(ancestorRect.left + padding, padding);
      }
      if (menuY + MENU_HEIGHT > viewportHeight - padding) {
        menuY = Math.min(
          ancestorRect.bottom - MENU_HEIGHT - padding,
          viewportHeight - MENU_HEIGHT - padding
        );
      }
    } else {
      // Place outside the ancestor
      const gap = 10;

      // Try to place to the right first
      menuX = ancestorRect.right + gap;
      menuY = ancestorRect.top;

      // If not enough space on right, try left
      if (menuX + MENU_WIDTH > viewportWidth - gap) {
        menuX = ancestorRect.left - MENU_WIDTH - gap;
      }

      // If still not enough space, place at viewport edge with minimum gap
      if (menuX < gap) {
        menuX = gap;
      }

      // Adjust Y to keep within viewport
      menuY = Math.max(
        gap,
        Math.min(menuY, viewportHeight - MENU_HEIGHT - gap)
      );
    }

    return {
      x: menuX + window.scrollX,
      y: menuY + window.scrollY,
    };
  },

  /**
   * Create the image menu DOM element
   * @param {Array} images - Array of image data objects
   * @param {number} x - X position for menu
   * @param {number} y - Y position for menu
   * @returns {Element} Menu DOM element
   */
  createImageMenu: function(images, x, y) {
    const menu = document.createElement("div");
    menu.className = "image-url-menu";
    menu.style.position = "absolute";
    menu.style.left = x + "px";
    menu.style.top = y + "px";
    menu.style.zIndex = "10001";

    images.forEach((imgData, index) => {
      const button = document.createElement("button");
      button.className = "image-url-copy-btn image-url-menu-item";

      const url = imgData.url;
      const filename = url.split("/").pop().split("?")[0];
      const displayName =
        filename.length > 25 ? filename.substring(0, 22) + "..." : filename;

      button.textContent = `${index + 1}. ${displayName}`;
      button.title = url;

      button.addEventListener("click", (e) => {
        e.preventDefault();
        e.stopPropagation();
        window.ImageExtension.clipboard.copyImageURL(url, button);
      });

      menu.appendChild(button);
    });

    return menu;
  },

  /**
   * Check if cursor is in menu transition zone
   * @param {number} x - X coordinate
   * @param {number} y - Y coordinate
   * @param {Element} currentMenu - Current menu element
   * @param {Element} currentAncestor - Current ancestor element
   * @returns {boolean} True if in transition zone
   */
  isInMenuTransitionZone: function(x, y, currentMenu, currentAncestor) {
    if (!currentMenu || !currentAncestor) return false;

    const menuRect = currentMenu.getBoundingClientRect();
    const ancestorRect = currentAncestor.getBoundingClientRect();

    // Calculate the bridge rectangle between ancestor and menu
    const bridgeRect = {
      left: Math.min(ancestorRect.left, menuRect.left),
      right: Math.max(ancestorRect.right, menuRect.right),
      top: Math.min(ancestorRect.top, menuRect.top),
      bottom: Math.max(ancestorRect.bottom, menuRect.bottom),
    };

    // Check if cursor is in the bridge area
    const inBridge =
      x >= bridgeRect.left &&
      x <= bridgeRect.right &&
      y >= bridgeRect.top &&
      y <= bridgeRect.bottom;

    // Also check if directly in menu or ancestor
    const inMenu =
      x >= menuRect.left &&
      x <= menuRect.right &&
      y >= menuRect.top &&
      y <= menuRect.bottom;

    const inAncestor =
      x >= ancestorRect.left &&
      x <= ancestorRect.right &&
      y >= ancestorRect.top &&
      y <= ancestorRect.bottom;

    return inBridge || inMenu || inAncestor;
  }
};