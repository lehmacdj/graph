// Main coordinator for Image Extension
// Manages state and coordinates between modules

(function () {
  // Extension state
  const state = {
    currentMenu: null,
    currentImages: [],
    currentHoverElement: null,
    currentAncestor: null,
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

  // Event handlers with state binding
  function handleMouseMove(e) {
    window.ImageExtension.eventHandlers.handleMouseMove(e, state);
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
