// Background script to handle toolbar icon clicks and manage preview toggle state

// Initialize the extension state (enabled by default)
chrome.runtime.onInstalled.addListener(() => {
  chrome.storage.local.get(['previewsEnabled'], (result) => {
    if (result.previewsEnabled === undefined) {
      chrome.storage.local.set({ previewsEnabled: true });
    }
  });
});

// Handle toolbar icon clicks to toggle preview state
chrome.browserAction.onClicked.addListener((tab) => {
  chrome.storage.local.get(['previewsEnabled'], (result) => {
    const currentState = result.previewsEnabled !== false; // Default to true
    const newState = !currentState;

    // Update storage
    chrome.storage.local.set({ previewsEnabled: newState }, () => {
      // Update icon to reflect state
      updateIcon(newState);

      // Notify content script of the change
      chrome.tabs.sendMessage(tab.id, {
        type: 'togglePreviews',
        enabled: newState
      });
    });
  });
});

// Update icon appearance based on state
function updateIcon(enabled) {
  const iconPath = enabled ? {
    "16": "icons/icon16.png",
    "32": "icons/icon32.png",
    "48": "icons/icon48.png"
  } : {
    "16": "icons/icon16-disabled.png",
    "32": "icons/icon32-disabled.png",
    "48": "icons/icon48-disabled.png"
  };

  const title = enabled ? "Image Preview (Enabled)" : "Image Preview (Disabled)";

  chrome.browserAction.setIcon({ path: iconPath });
  chrome.browserAction.setTitle({ title: title });
}

// Set initial icon state when extension loads
chrome.storage.local.get(['previewsEnabled'], (result) => {
  const enabled = result.previewsEnabled !== false;
  updateIcon(enabled);
});
