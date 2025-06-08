// Clipboard operations and user feedback
window.ImageExtension = window.ImageExtension || {};
window.ImageExtension.clipboard = {
  /**
   * Copy an image URL to clipboard with fallback
   * @param {string} url - URL to copy
   * @param {Element} button - Button element for feedback
   */
  copyImageURL: function(url, button) {
    if (url) {
      navigator.clipboard
        .writeText(url)
        .then(() => {
          this.showCopyFeedback(button);
        })
        .catch(() => {
          this.fallbackCopyTextToClipboard(url, button);
        });
    }
  },

  /**
   * Fallback clipboard copy using deprecated execCommand
   * @param {string} text - Text to copy
   * @param {Element} button - Button element for feedback
   */
  fallbackCopyTextToClipboard: function(text, button) {
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
      this.showCopyFeedback(button);
    } catch (err) {
      console.error("Failed to copy: ", err);
    }

    document.body.removeChild(textArea);
  },

  /**
   * Show visual feedback when copy operation succeeds
   * @param {Element} button - Button element to show feedback on
   */
  showCopyFeedback: function(button) {
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
};