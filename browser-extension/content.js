(function () {
  let currentMenu = null;
  let currentImages = [];
  let currentHoverElement = null;
  let currentAncestor = null;

  function getImageURL(element) {
    if (element.tagName === "IMG") {
      return element.src || element.currentSrc;
    } else if (element.tagName === "image") {
      return element.href.baseVal || element.getAttribute("href");
    }
    return null;
  }

  function findCommonAncestor(elements) {
    if (elements.length === 0) return null;
    if (elements.length === 1) return elements[0];

    let commonAncestor = elements[0];
    for (let i = 1; i < elements.length; i++) {
      commonAncestor = findClosestCommonAncestor(commonAncestor, elements[i]);
    }
    return commonAncestor;
  }

  function findClosestCommonAncestor(element1, element2) {
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
  }

  function isElementVisible(element) {
    const style = window.getComputedStyle(element);
    return (
      style.visibility !== "hidden" &&
      style.display !== "none" &&
      (element.parentElement === null ||
        isElementVisible(element.parentElement))
    );
  }

  function intersectRects(rect1, rect2) {
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
      height,
    };
  }

  // Find the outermost SVG ancestor of an element
  // If it is possible for HTML to be nested inside SVG this might not be
  // accurate
  function outermostSVGAncestor(element) {
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

  function getBoundingRect(element) {
    let rect = element.getBoundingClientRect();
    if (element.tagName === "image") {
      // intersect the rect with the SVG element's bounding box
      const svgParent = outermostSVGAncestor(element);
      if (svgParent) {
        const svgRect = svgParent.getBoundingClientRect();
        const clippedRect = intersectRects(rect, svgRect);
        // Only return clipped rect if it has valid dimensions
        if (clippedRect.width > 0 && clippedRect.height > 0) {
          rect = clippedRect;
        } else {
          rect = svgRect; // Fallback to SVG bounding box if no intersection
        }
      }
    }
    return rect;
  }

  function findImagesInContainer(container, point = null) {
    let images = Array.from(container.querySelectorAll("img, image")).map(
      (img) => ({
        element: img,
        url: getImageURL(img),
        rect: getBoundingRect(img),
      })
    );

    images = images.filter((img) => img.url && isElementVisible(img.element));

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

    return removeDuplicateUrls(images);
  }

  function removeDuplicateUrls(images) {
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
  }

  function findImagesAtPoint(x, y) {
    const result = findImagesInContainer(document, { x, y });
    return result;
  }

  function findAncestorAndImages(imagesAtPoint) {
    const ancestor = findCommonAncestor(
      imagesAtPoint.map((img) => img.element)
    );
    const allImagesInAncestor = findImagesInContainer(ancestor);
    return { ancestor, images: allImagesInAncestor };
  }

  function calculateMenuPosition(ancestor, images) {
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
  }

  function createImageMenu(images, x, y) {
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
        copyImageURL(url, button);
      });

      menu.appendChild(button);
    });

    return menu;
  }

  function copyImageURL(url, button) {
    if (url) {
      navigator.clipboard
        .writeText(url)
        .then(() => {
          showCopyFeedback(button);
        })
        .catch(() => {
          fallbackCopyTextToClipboard(url, button);
        });
    }
  }

  function fallbackCopyTextToClipboard(text, button) {
    const textArea = document.createElement("textarea");
    textArea.value = text;
    textArea.style.position = "fixed";
    textArea.style.left = "-999999px";
    textArea.style.top = "-999999px";
    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();

    try {
      document.execCommand("copy");
      showCopyFeedback(button);
    } catch (err) {
      console.error("Failed to copy: ", err);
    }

    document.body.removeChild(textArea);
  }

  function showCopyFeedback(button) {
    if (button) {
      const originalText = button.textContent;
      button.textContent = "Copied!";
      button.style.backgroundColor = "#4CAF50";

      setTimeout(() => {
        if (button && button.parentNode) {
          button.textContent = originalText;
          button.style.backgroundColor = "";
        }
      }, 1000);
    }
  }

  function removeMenu() {
    if (currentMenu) {
      currentMenu.remove();
      currentMenu = null;
    }

    // Force reset all state
    currentImages = [];
    currentHoverElement = null;
    currentAncestor = null;
  }

  function isInMenuTransitionZone(x, y) {
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

  function handleMouseMove(e) {
    const x = e.clientX;
    const y = e.clientY;

    // Don't dismiss menu if hovering over it or in transition zone
    if (
      currentMenu &&
      (currentMenu.contains(e.target) || isInMenuTransitionZone(x, y))
    ) {
      return;
    }

    const imagesAtPoint = findImagesAtPoint(x, y);

    if (imagesAtPoint.length === 0) {
      // Only remove menu if we're not in the transition zone
      if (!isInMenuTransitionZone(x, y)) {
        removeMenu();
      }
      return;
    }

    // Find ancestor and validate it
    const { ancestor, images: allImagesInAncestor } =
      findAncestorAndImages(imagesAtPoint);

    // Check if we're still within the same ancestor element with same images
    if (
      currentAncestor === ancestor &&
      currentImages.length === allImagesInAncestor.length &&
      currentImages.every(
        (img, i) => img.element === allImagesInAncestor[i].element
      )
    ) {
      // Validate we're still in a reasonable area
      const ancestorRect = currentAncestor.getBoundingClientRect();
      if (
        x >= ancestorRect.left &&
        x <= ancestorRect.right &&
        y >= ancestorRect.top &&
        y <= ancestorRect.bottom
      ) {
        return;
      }
    }

    updateMenuForImages(ancestor, allImagesInAncestor, imagesAtPoint);
  }

  function updateMenuForImages(ancestor, allImagesInAncestor, imagesAtPoint) {
    removeMenu();

    currentImages = allImagesInAncestor;
    currentHoverElement =
      imagesAtPoint.length > 0 ? imagesAtPoint[0].element : null;
    currentAncestor = ancestor;

    // Use smart positioning
    const { x: menuX, y: menuY } = calculateMenuPosition(
      ancestor,
      allImagesInAncestor
    );

    currentMenu = createImageMenu(allImagesInAncestor, menuX, menuY);
    document.body.appendChild(currentMenu);
  }

  function handleMouseLeave(e) {
    const relatedTarget = e.relatedTarget;

    if (
      currentMenu &&
      (relatedTarget === currentMenu || currentMenu.contains(relatedTarget))
    ) {
      return;
    }

    if (
      currentAncestor &&
      (relatedTarget === currentAncestor ||
        currentAncestor.contains(relatedTarget))
    ) {
      return;
    }

    setTimeout(() => {
      if (currentMenu && !currentMenu.matches(":hover")) {
        removeMenu();
      }
    }, 100);
  }

  document.addEventListener("mousemove", handleMouseMove);

  document.addEventListener("mouseleave", function (e) {
    if (e.target === currentMenu) {
      handleMouseLeave(e);
    }
  });

  document.addEventListener("click", function (e) {
    if (currentMenu && !currentMenu.contains(e.target)) {
      removeMenu();
    }
  });

  // Force cleanup on page visibility change
  document.addEventListener("visibilitychange", function () {
    if (document.hidden) {
      removeMenu();
    }
  });

  // Cleanup on navigation
  window.addEventListener("beforeunload", function () {
    removeMenu();
  });

  document.addEventListener("scroll", function () {
    if (currentMenu && currentAncestor && currentImages.length > 0) {
      const { x: menuX, y: menuY } = calculateMenuPosition(
        currentAncestor,
        currentImages
      );
      currentMenu.style.left = menuX + "px";
      currentMenu.style.top = menuY + "px";
    }
  });

  window.addEventListener("resize", function () {
    if (currentMenu && currentAncestor && currentImages.length > 0) {
      const { x: menuX, y: menuY } = calculateMenuPosition(
        currentAncestor,
        currentImages
      );
      currentMenu.style.left = menuX + "px";
      currentMenu.style.top = menuY + "px";
    }
  });
})();
