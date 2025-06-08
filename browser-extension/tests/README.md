# Image Extension Test Suite

## Overview

This automated test suite validates the Image Extension functionality using simulated mouse events and DOM assertions.

## Test Files

- **`test-framework.js`** - Core testing framework with utilities
- **`extension-tests.js`** - Comprehensive test cases for the extension
- **`test-automated.html`** - Interactive test runner page

## Running Tests

### Browser Testing
1. Open `test-automated.html` in your browser
2. Click "▶️ Run All Tests" button
3. Watch real-time test execution and results

### Extension Testing
1. Load the browser extension
2. Navigate to any webpage with images
3. Hover over images to verify functionality

## Test Cases

### Basic Functionality
- ✅ Menu appears on image hover
- ✅ Menu contains correct image URLs
- ✅ Menu positioning is correct
- ✅ Menu disappears when mouse leaves

### Advanced Features
- ✅ Multiple images in containers
- ✅ SVG image detection
- ✅ SVG clipping boundaries
- ✅ Rapid mouse movement handling
- ✅ Extension state consistency
- ✅ Clipboard functionality

### Edge Cases
- ✅ Clipped SVG images
- ✅ Mixed content containers
- ✅ Performance under stress
- ✅ State cleanup

## Framework Features

### Assertions
```javascript
assert.equals(actual, expected, message)
assert.truthy(value, message)
assert.falsy(value, message)
assert.elementExists(selector, message)
assert.elementCount(selector, expectedCount, message)
assert.contains(array, item, message)
```

### DOM Utilities
```javascript
dom.simulateMouseEvent(eventType, x, y, target)
dom.getElementCenter(element)
dom.waitFor(condition, timeout, interval)
dom.clearMenus()
dom.getExtensionState()
```

### Writing New Tests
```javascript
test('Test description', async function() {
  // Setup
  dom.clearMenus();
  
  // Action
  const img = assert.elementExists('#test-image');
  const center = dom.getElementCenter(img);
  dom.simulateMouseEvent('mousemove', center.x, center.y);
  
  // Wait for result
  await dom.waitFor(() => document.querySelector('.image-url-menu'));
  
  // Assert
  const menu = assert.elementExists('.image-url-menu');
  assert.truthy(menu, 'Menu should appear');
});
```

## Debugging

- Open browser DevTools console to see detailed test output
- Test results show pass/fail status with error messages
- Extension state can be inspected using `dom.getExtensionState()`
- Individual tests can be run by calling them directly

## CI/CD Integration

The test framework can be integrated with headless browsers for automated testing:

```bash
# Example with Playwright
npx playwright test --headed test-automated.html
```

## Performance Considerations

- Tests include timing assertions to ensure responsive behavior
- Rapid mouse movement simulation tests performance under stress
- Memory leak detection through state consistency checks