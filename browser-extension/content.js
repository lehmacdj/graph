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

  function findImagesAtPoint(x, y) {
    const elements = [];
    const allImages = document.querySelectorAll("img, image");

    for (const img of allImages) {
      const rect = img.getBoundingClientRect();
      if (
        x >= rect.left &&
        x <= rect.right &&
        y >= rect.top &&
        y <= rect.bottom
      ) {
        const url = getImageURL(img);
        if (url) {
          elements.push({ element: img, url, rect });
        }
      }
    }

    return elements.sort((a, b) => {
      const aZIndex = parseInt(window.getComputedStyle(a.element).zIndex) || 0;
      const bZIndex = parseInt(window.getComputedStyle(b.element).zIndex) || 0;
      return bZIndex - aZIndex;
    });
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

    currentImages.forEach((imgData) => {
      imgData.element.classList.remove("image-url-outlined");
    });
    currentImages = [];
    currentHoverElement = null;
    currentAncestor = null;
  }

  function handleMouseMove(e) {
    const x = e.clientX;
    const y = e.clientY;

    const imagesAtPoint = findImagesAtPoint(x, y);

    if (imagesAtPoint.length === 0) {
      removeMenu();
      return;
    }

    // Find ancestor and get all images within it
    const ancestor = findCommonAncestor(
      imagesAtPoint.map((img) => img.element)
    );
    const allImagesInAncestor = Array.from(
      ancestor.querySelectorAll("img, image")
    )
      .map((img) => ({
        element: img,
        url: getImageURL(img),
        rect: img.getBoundingClientRect(),
      }))
      .filter((img) => img.url)
      .sort((a, b) => {
        const aZIndex =
          parseInt(window.getComputedStyle(a.element).zIndex) || 0;
        const bZIndex =
          parseInt(window.getComputedStyle(b.element).zIndex) || 0;
        return bZIndex - aZIndex;
      });

    // Check if we're still within the same ancestor element with same images
    if (
      currentAncestor === ancestor &&
      currentImages.length === allImagesInAncestor.length &&
      currentImages.every(
        (img, i) => img.element === allImagesInAncestor[i].element
      )
    ) {
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

    removeMenu();

    currentImages = allImagesInAncestor;
    currentHoverElement = imagesAtPoint[0].element;
    currentAncestor = ancestor;

    allImagesInAncestor.forEach((imgData) => {
      imgData.element.classList.add("image-url-outlined");
    });

    // Position menu within ancestor bounds
    const ancestorRect = currentAncestor.getBoundingClientRect();
    const menuX = Math.max(
      ancestorRect.left + window.scrollX + 10,
      Math.min(
        ancestorRect.right + window.scrollX - 220,
        window.innerWidth + window.scrollX - 220
      )
    );
    const menuY = Math.max(
      ancestorRect.top + window.scrollY + 10,
      Math.min(
        ancestorRect.bottom + window.scrollY - 50,
        window.innerHeight + window.scrollY - 50
      )
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

  document.addEventListener("scroll", function () {
    if (currentMenu && currentAncestor) {
      const ancestorRect = currentAncestor.getBoundingClientRect();
      const menuX = Math.max(
        ancestorRect.left + window.scrollX + 10,
        Math.min(
          ancestorRect.right + window.scrollX - 220,
          window.innerWidth + window.scrollX - 220
        )
      );
      const menuY = Math.max(
        ancestorRect.top + window.scrollY + 10,
        Math.min(
          ancestorRect.bottom + window.scrollY - 50,
          window.innerHeight + window.scrollY - 50
        )
      );

      currentMenu.style.left = menuX + "px";
      currentMenu.style.top = menuY + "px";
    }
  });

  window.addEventListener("resize", function () {
    if (currentMenu && currentAncestor) {
      const ancestorRect = currentAncestor.getBoundingClientRect();
      const menuX = Math.max(
        ancestorRect.left + window.scrollX + 10,
        Math.min(
          ancestorRect.right + window.scrollX - 220,
          window.innerWidth + window.scrollX - 220
        )
      );
      const menuY = Math.max(
        ancestorRect.top + window.scrollY + 10,
        Math.min(
          ancestorRect.bottom + window.scrollY - 50,
          window.innerHeight + window.scrollY - 50
        )
      );

      currentMenu.style.left = menuX + "px";
      currentMenu.style.top = menuY + "px";
    }
  });
})();
