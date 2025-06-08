// Comprehensive test cases for Image Extension
(function() {
  const { test, assert, dom } = window.TestFramework;

  // Test: Basic hover on single image
  test('Should show menu when hovering over single image', async function() {
    dom.clearMenus();
    
    const img = assert.elementExists('#test-img-1', 'Test image 1 should exist');
    const center = dom.getElementCenter(img);
    
    // Simulate mouse move to image center
    dom.simulateMouseEvent('mousemove', center.x, center.y);
    
    // Wait for menu to appear
    await dom.waitFor(() => document.querySelector('.image-url-menu'));
    
    const menu = assert.elementExists('.image-url-menu', 'Menu should appear');
    const buttons = menu.querySelectorAll('.image-url-copy-btn');
    
    assert.truthy(buttons.length >= 1, 'Menu should have at least one button');
    
    const state = dom.getExtensionState();
    assert.truthy(state.currentMenu, 'Extension state should have current menu');
    assert.truthy(state.currentImages.length >= 1, 'Extension should track current images');
  });

  // Test: Menu contains correct image URL
  test('Menu should contain correct image URL', async function() {
    dom.clearMenus();
    
    const img = assert.elementExists('#test-img-2', 'Test image 2 should exist');
    const expectedUrl = img.src;
    const center = dom.getElementCenter(img);
    
    dom.simulateMouseEvent('mousemove', center.x, center.y);
    await dom.waitFor(() => document.querySelector('.image-url-menu'));
    
    const menu = assert.elementExists('.image-url-menu');
    const button = menu.querySelector('.image-url-copy-btn');
    
    assert.truthy(button, 'Menu should have copy button');
    assert.equals(button.title, expectedUrl, 'Button title should match image URL');
  });

  // Test: Multiple images in container
  test('Should show menu with multiple images when hovering over container', async function() {
    dom.clearMenus();
    
    const container = assert.elementExists('#multi-image-container', 'Multi-image container should exist');
    const images = container.querySelectorAll('img, image');
    const expectedCount = images.length;
    
    // Hover over first image in container
    const firstImg = images[0];
    const center = dom.getElementCenter(firstImg);
    
    dom.simulateMouseEvent('mousemove', center.x, center.y);
    await dom.waitFor(() => document.querySelector('.image-url-menu'));
    
    const menu = assert.elementExists('.image-url-menu');
    const buttons = menu.querySelectorAll('.image-url-copy-btn');
    
    assert.equals(buttons.length, expectedCount, 
      `Menu should show ${expectedCount} buttons for ${expectedCount} images`);
  });

  // Test: SVG image detection
  test('Should detect SVG images correctly', async function() {
    dom.clearMenus();
    
    const svgImg = assert.elementExists('#test-svg-image', 'SVG image should exist');
    const center = dom.getElementCenter(svgImg);
    
    dom.simulateMouseEvent('mousemove', center.x, center.y);
    await dom.waitFor(() => document.querySelector('.image-url-menu'));
    
    const menu = assert.elementExists('.image-url-menu');
    const button = menu.querySelector('.image-url-copy-btn');
    
    assert.truthy(button, 'Menu should appear for SVG image');
    assert.truthy(button.title.length > 0, 'SVG image should have valid URL');
  });

  // Test: SVG clipping behavior
  test('Should respect SVG clipping boundaries', async function() {
    dom.clearMenus();
    
    const clippedSvg = assert.elementExists('#clipped-svg', 'Clipped SVG should exist');
    const svgRect = clippedSvg.getBoundingClientRect();
    
    // Test point outside SVG bounds but potentially inside image bounds
    const outsidePoint = {
      x: svgRect.right + 10,
      y: svgRect.top + 10
    };
    
    dom.simulateMouseEvent('mousemove', outsidePoint.x, outsidePoint.y);
    
    // Wait a moment to see if menu appears (it shouldn't)
    await new Promise(resolve => setTimeout(resolve, 200));
    
    const menu = document.querySelector('.image-url-menu');
    assert.falsy(menu, 'Menu should not appear outside SVG clipping bounds');
  });

  // Test: Menu positioning
  test('Should position menu correctly relative to images', async function() {
    dom.clearMenus();
    
    const img = assert.elementExists('#test-img-1', 'Test image should exist');
    const center = dom.getElementCenter(img);
    
    dom.simulateMouseEvent('mousemove', center.x, center.y);
    await dom.waitFor(() => document.querySelector('.image-url-menu'));
    
    const menu = assert.elementExists('.image-url-menu');
    const menuRect = menu.getBoundingClientRect();
    const imgRect = img.getBoundingClientRect();
    
    // Menu should be positioned somewhere reasonable relative to the image
    const isReasonablyPositioned = 
      (menuRect.left >= imgRect.right) || // To the right
      (menuRect.right <= imgRect.left) || // To the left
      (menuRect.top >= imgRect.bottom) || // Below
      (menuRect.bottom <= imgRect.top);   // Above
    
    assert.truthy(isReasonablyPositioned, 
      'Menu should be positioned adjacent to or within the image area');
  });

  // Test: Menu removal on mouse leave
  test('Should remove menu when mouse leaves image area', async function() {
    dom.clearMenus();
    
    const img = assert.elementExists('#test-img-1', 'Test image should exist');
    const center = dom.getElementCenter(img);
    
    // Show menu
    dom.simulateMouseEvent('mousemove', center.x, center.y);
    await dom.waitFor(() => document.querySelector('.image-url-menu'));
    
    assert.elementExists('.image-url-menu', 'Menu should be visible');
    
    // Move mouse far away
    dom.simulateMouseEvent('mousemove', 50, 50);
    
    // Wait for menu to disappear
    await dom.waitFor(() => !document.querySelector('.image-url-menu'), 2000);
    
    const menu = document.querySelector('.image-url-menu');
    assert.falsy(menu, 'Menu should disappear when mouse leaves');
  });

  // Test: Click simulation and clipboard functionality
  test('Should handle menu button clicks', async function() {
    dom.clearMenus();
    
    const img = assert.elementExists('#test-img-1', 'Test image should exist');
    const center = dom.getElementCenter(img);
    
    dom.simulateMouseEvent('mousemove', center.x, center.y);
    await dom.waitFor(() => document.querySelector('.image-url-menu'));
    
    const menu = assert.elementExists('.image-url-menu');
    const button = menu.querySelector('.image-url-copy-btn');
    
    assert.truthy(button, 'Menu should have copy button');
    
    // Click the button
    button.click();
    
    // Wait for feedback
    await dom.waitFor(() => button.textContent.includes('Copied'), 2000);
    
    assert.truthy(button.textContent.includes('Copied'), 
      'Button should show copy feedback');
  });

  // Test: Extension state consistency
  test('Should maintain consistent internal state', async function() {
    dom.clearMenus();
    
    const img = assert.elementExists('#test-img-1', 'Test image should exist');
    const center = dom.getElementCenter(img);
    
    // Initial state should be clean
    let state = dom.getExtensionState();
    assert.falsy(state.currentMenu, 'Initial state should have no menu');
    assert.equals(state.currentImages.length, 0, 'Initial state should have no images');
    
    // Show menu
    dom.simulateMouseEvent('mousemove', center.x, center.y);
    await dom.waitFor(() => document.querySelector('.image-url-menu'));
    
    // State should be populated
    state = dom.getExtensionState();
    assert.truthy(state.currentMenu, 'State should have current menu');
    assert.truthy(state.currentImages.length > 0, 'State should have current images');
    assert.truthy(state.currentAncestor, 'State should have current ancestor');
    
    // Clear menu
    dom.simulateMouseEvent('mousemove', 50, 50);
    await dom.waitFor(() => !document.querySelector('.image-url-menu'));
    
    // State should be clean again
    state = dom.getExtensionState();
    assert.falsy(state.currentMenu, 'State should be clean after menu removal');
    assert.equals(state.currentImages.length, 0, 'State should have no images after cleanup');
  });

  // Test: Performance with rapid mouse movements
  test('Should handle rapid mouse movements without errors', async function() {
    dom.clearMenus();
    
    const img1 = assert.elementExists('#test-img-1');
    const img2 = assert.elementExists('#test-img-2');
    const center1 = dom.getElementCenter(img1);
    const center2 = dom.getElementCenter(img2);
    
    // Rapidly move between images
    for (let i = 0; i < 10; i++) {
      dom.simulateMouseEvent('mousemove', center1.x, center1.y);
      await new Promise(resolve => setTimeout(resolve, 10));
      dom.simulateMouseEvent('mousemove', center2.x, center2.y);
      await new Promise(resolve => setTimeout(resolve, 10));
    }
    
    // Should end up with a menu for the last hovered image
    await dom.waitFor(() => document.querySelector('.image-url-menu'));
    
    const menu = assert.elementExists('.image-url-menu');
    assert.truthy(menu, 'Menu should still work after rapid movements');
    
    const state = dom.getExtensionState();
    assert.truthy(state.currentMenu, 'Extension state should be consistent');
  });

})();