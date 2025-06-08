// Automated testing framework for Image Extension
window.TestFramework = {
  tests: [],
  results: [],
  
  /**
   * Register a test case
   * @param {string} name - Test name
   * @param {Function} testFn - Test function
   */
  test: function(name, testFn) {
    this.tests.push({ name, testFn });
  },

  /**
   * Run all registered tests
   */
  runAll: async function() {
    console.log('🧪 Starting Image Extension Tests...');
    this.results = [];
    
    for (const test of this.tests) {
      try {
        console.log(`Running: ${test.name}`);
        await test.testFn();
        this.results.push({ name: test.name, status: 'PASS', error: null });
        console.log(`✅ PASS: ${test.name}`);
      } catch (error) {
        this.results.push({ name: test.name, status: 'FAIL', error: error.message });
        console.error(`❌ FAIL: ${test.name} - ${error.message}`);
      }
    }
    
    this.printSummary();
  },

  /**
   * Print test results summary
   */
  printSummary: function() {
    const passed = this.results.filter(r => r.status === 'PASS').length;
    const failed = this.results.filter(r => r.status === 'FAIL').length;
    
    console.log('\n📊 Test Summary:');
    console.log(`Total: ${this.results.length}`);
    console.log(`✅ Passed: ${passed}`);
    console.log(`❌ Failed: ${failed}`);
    
    if (failed > 0) {
      console.log('\n💥 Failed Tests:');
      this.results.filter(r => r.status === 'FAIL').forEach(r => {
        console.log(`  - ${r.name}: ${r.error}`);
      });
    }
  },

  /**
   * Assertion helpers
   */
  assert: {
    equals: function(actual, expected, message = '') {
      if (actual !== expected) {
        throw new Error(`${message} Expected: ${expected}, Actual: ${actual}`);
      }
    },

    truthy: function(value, message = '') {
      if (!value) {
        throw new Error(`${message} Expected truthy value, got: ${value}`);
      }
    },

    falsy: function(value, message = '') {
      if (value) {
        throw new Error(`${message} Expected falsy value, got: ${value}`);
      }
    },

    elementExists: function(selector, message = '') {
      const element = document.querySelector(selector);
      if (!element) {
        throw new Error(`${message} Element not found: ${selector}`);
      }
      return element;
    },

    elementCount: function(selector, expectedCount, message = '') {
      const elements = document.querySelectorAll(selector);
      if (elements.length !== expectedCount) {
        throw new Error(`${message} Expected ${expectedCount} elements, found ${elements.length}`);
      }
      return elements;
    },

    contains: function(array, item, message = '') {
      if (!array.includes(item)) {
        throw new Error(`${message} Array does not contain: ${item}`);
      }
    }
  },

  /**
   * DOM utilities for testing
   */
  dom: {
    /**
     * Simulate mouse event at coordinates
     * @param {string} eventType - Event type (mousemove, click, etc.)
     * @param {number} x - X coordinate
     * @param {number} y - Y coordinate
     * @param {Element} target - Target element (optional)
     */
    simulateMouseEvent: function(eventType, x, y, target = document.elementFromPoint(x, y)) {
      const event = new MouseEvent(eventType, {
        clientX: x,
        clientY: y,
        bubbles: true,
        cancelable: true,
        view: window
      });
      
      if (target) {
        target.dispatchEvent(event);
      } else {
        document.dispatchEvent(event);
      }
      
      return event;
    },

    /**
     * Get element center coordinates
     * @param {Element} element - DOM element
     * @returns {Object} Object with x, y coordinates
     */
    getElementCenter: function(element) {
      const rect = element.getBoundingClientRect();
      return {
        x: rect.left + rect.width / 2,
        y: rect.top + rect.height / 2
      };
    },

    /**
     * Wait for a condition to be true
     * @param {Function} condition - Function that returns boolean
     * @param {number} timeout - Timeout in milliseconds
     * @param {number} interval - Check interval in milliseconds
     */
    waitFor: function(condition, timeout = 5000, interval = 100) {
      return new Promise((resolve, reject) => {
        const startTime = Date.now();
        
        const check = () => {
          if (condition()) {
            resolve();
          } else if (Date.now() - startTime > timeout) {
            reject(new Error('Timeout waiting for condition'));
          } else {
            setTimeout(check, interval);
          }
        };
        
        check();
      });
    },

    /**
     * Clear any existing menus
     */
    clearMenus: function() {
      const menus = document.querySelectorAll('.image-url-menu');
      menus.forEach(menu => menu.remove());
    },

    /**
     * Get current extension state
     */
    getExtensionState: function() {
      // Access internal state through a test hook we'll add
      return window.ImageExtension.testHooks?.getState() || {};
    }
  }
};