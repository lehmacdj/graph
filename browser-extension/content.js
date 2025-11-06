// Main coordinator for Image Extension
// Manages state and coordinates between modules

(function () {
  // Extension state
  const state = {
    currentMenu: null,
    currentImages: [],
    currentHoverElement: null,
    currentAncestor: null,
    previewsEnabled: true, // Default to enabled
  };

  // Wait for all modules to be loaded
  if (
    !window.ImageExtension ||
    !window.ImageExtension.eventHandlers ||
    !window.ImageExtension.imageDetection ||
    !window.ImageExtension.menu ||
    !window.ImageExtension.clipboard ||
    !window.ImageExtension.domUtils ||
    !window.ImageExtension.geometry
  ) {
    console.error("ImageExtension modules not fully loaded");
    return;
  }

  // Initialize preview state from storage
  chrome.storage.local.get(['previewsEnabled'], (result) => {
    state.previewsEnabled = result.previewsEnabled !== false; // Default to true
  });

  // Listen for toggle messages from background script
  chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
    if (message.type === 'togglePreviews') {
      state.previewsEnabled = message.enabled;

      // If disabled, remove any existing menu
      if (!state.previewsEnabled) {
        window.ImageExtension.eventHandlers.removeMenu(state);
      }
    }
  });

  // Event handlers with state binding
  function handleMouseMove(e) {
    // Only process mouse movement if previews are enabled
    if (state.previewsEnabled) {
      window.ImageExtension.eventHandlers.handleMouseMove(e, state);
    }
  }

  function handleMouseLeave(e) {
    if (e.target === state.currentMenu) {
      window.ImageExtension.eventHandlers.handleMouseLeave(e, state);
    }
  }

  function handleClick(e) {
    if (state.currentMenu && !state.currentMenu.contains(e.target)) {
      window.ImageExtension.eventHandlers.removeMenu(state);
    }
  }

  function handleVisibilityChange() {
    if (document.hidden) {
      window.ImageExtension.eventHandlers.removeMenu(state);
    }
  }

  function handleBeforeUnload() {
    window.ImageExtension.eventHandlers.removeMenu(state);
  }

  function handleScroll() {
    window.ImageExtension.eventHandlers.handleScroll(state);
  }

  function handleResize() {
    window.ImageExtension.eventHandlers.handleResize(state);
  }

  // Register event listeners
  document.addEventListener("mousemove", handleMouseMove);
  document.addEventListener("mouseleave", handleMouseLeave);
  document.addEventListener("click", handleClick);
  document.addEventListener("visibilitychange", handleVisibilityChange);
  window.addEventListener("beforeunload", handleBeforeUnload);
  document.addEventListener("scroll", handleScroll);
  window.addEventListener("resize", handleResize);

  console.log("Image Extension initialized with modular structure");
})();
