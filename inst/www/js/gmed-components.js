/**
 * GMED Components JavaScript
 * Supporting functions for GMED UI components
 */

// Progress bar utilities
window.gmedProgressBar = {
  
  /**
   * Update progress bar value with animation
   * @param {string} id - The ID of the progress bar element
   * @param {number} value - Progress value (0-100)
   * @param {boolean} animated - Whether to show animation
   */
  updateProgress: function(id, value, animated = false) {
    const progressBar = document.getElementById(id);
    if (!progressBar) return;
    
    // Ensure value is between 0 and 100
    value = Math.max(0, Math.min(100, value));
    
    // Add or remove animation class
    if (animated) {
      progressBar.classList.add('animated');
    } else {
      progressBar.classList.remove('animated');
    }
    
    // Update width with smooth transition
    progressBar.style.width = value + '%';
    progressBar.setAttribute('aria-valuenow', value);
  },
  
  /**
   * Set progress bar to indeterminate state
   * @param {string} id - The ID of the progress bar element
   */
  setIndeterminate: function(id) {
    const progressBar = document.getElementById(id);
    if (!progressBar) return;
    
    progressBar.classList.add('animated');
    progressBar.style.width = '100%';
    progressBar.removeAttribute('aria-valuenow');
  }
};

// Status badge utilities
window.gmedStatusBadge = {
  
  /**
   * Update status badge content and styling
   * @param {string} selector - CSS selector for the badge element
   * @param {string} text - New badge text
   * @param {string} status - Status type: complete, incomplete, in-progress, pending
   */
  updateStatus: function(selector, text, status) {
    const badge = document.querySelector(selector);
    if (!badge) return;
    
    // Remove existing status classes
    badge.classList.remove('gmed-status-complete', 'gmed-status-incomplete', 
                          'gmed-status-in-progress', 'gmed-status-pending');
    
    // Add new status class
    const statusClass = 'gmed-status-' + status;
    badge.classList.add(statusClass);
    
    // Update text
    badge.textContent = text;
  }
};

// Selector container utilities
window.gmedSelector = {
  
  /**
   * Focus on selectize input when container is clicked
   * @param {string} containerId - ID of the selector container
   * @param {string} selectizeId - ID of the selectize input
   */
  setupFocus: function(containerId, selectizeId) {
    const container = document.getElementById(containerId);
    const selectize = document.getElementById(selectizeId);
    
    if (!container || !selectize) return;
    
    container.addEventListener('click', function(e) {
      // Don't trigger if clicking directly on the input
      if (e.target === selectize || selectize.contains(e.target)) return;
      
      // Focus the selectize input
      if (selectize.selectize) {
        selectize.selectize.focus();
      } else {
        selectize.focus();
      }
    });
  }
};

// Card utilities
window.gmedCard = {
  
  /**
   * Add loading state to card
   * @param {string} selector - CSS selector for the card
   */
  setLoading: function(selector) {
    const card = document.querySelector(selector);
    if (!card) return;
    
    card.classList.add('gmed-loading');
    
    // Add loading spinner if not present
    let spinner = card.querySelector('.gmed-spinner');
    if (!spinner) {
      spinner = document.createElement('div');
      spinner.className = 'gmed-spinner';
      spinner.innerHTML = '<i class="fas fa-spinner fa-spin"></i>';
      card.appendChild(spinner);
    }
  },
  
  /**
   * Remove loading state from card
   * @param {string} selector - CSS selector for the card
   */
  clearLoading: function(selector) {
    const card = document.querySelector(selector);
    if (!card) return;
    
    card.classList.remove('gmed-loading');
    
    // Remove loading spinner
    const spinner = card.querySelector('.gmed-spinner');
    if (spinner) {
      spinner.remove();
    }
  }
};

// Plus/Delta display utilities
window.gmedPlusDelta = {
  
  /**
   * Update plus/delta content
   * @param {string} containerId - ID of the plus/delta container
   * @param {string} plusText - Plus feedback text
   * @param {string} deltaText - Delta feedback text
   */
  updateContent: function(containerId, plusText, deltaText) {
    const container = document.getElementById(containerId);
    if (!container) return;
    
    const plusSection = container.querySelector('.gmed-plus-section .feedback-text');
    const deltaSection = container.querySelector('.gmed-delta-section .feedback-text');
    
    if (plusSection) {
      plusSection.textContent = plusText || 'No strengths feedback provided';
    }
    
    if (deltaSection) {
      deltaSection.textContent = deltaText || 'No improvement feedback provided';
    }
  }
};

// Animation utilities
window.gmedAnimations = {
  
  /**
   * Fade in element
   * @param {string} selector - CSS selector for the element
   * @param {number} duration - Animation duration in ms (default: 300)
   */
  fadeIn: function(selector, duration = 300) {
    const element = document.querySelector(selector);
    if (!element) return;
    
    element.style.opacity = '0';
    element.style.display = 'block';
    
    const start = performance.now();
    
    function animate(currentTime) {
      const elapsed = currentTime - start;
      const progress = Math.min(elapsed / duration, 1);
      
      element.style.opacity = progress;
      
      if (progress < 1) {
        requestAnimationFrame(animate);
      }
    }
    
    requestAnimationFrame(animate);
  },
  
  /**
   * Fade out element
   * @param {string} selector - CSS selector for the element
   * @param {number} duration - Animation duration in ms (default: 300)
   */
  fadeOut: function(selector, duration = 300) {
    const element = document.querySelector(selector);
    if (!element) return;
    
    const start = performance.now();
    const startOpacity = parseFloat(window.getComputedStyle(element).opacity);
    
    function animate(currentTime) {
      const elapsed = currentTime - start;
      const progress = Math.min(elapsed / duration, 1);
      
      element.style.opacity = startOpacity * (1 - progress);
      
      if (progress >= 1) {
        element.style.display = 'none';
      } else {
        requestAnimationFrame(animate);
      }
    }
    
    requestAnimationFrame(animate);
  }
};

// Initialize GMED components when document is ready
document.addEventListener('DOMContentLoaded', function() {
  // Auto-focus selectize inputs in selector containers
  document.querySelectorAll('.gmed-selector-container').forEach(function(container) {
    const selectize = container.querySelector('select[data-selectize]');
    if (selectize && container.id && selectize.id) {
      window.gmedSelector.setupFocus(container.id, selectize.id);
    }
  });
  
  // Initialize any other components as needed
  console.log('GMED components initialized');
});

