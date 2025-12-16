// www/sidebar.js

// 1) Handle clicks on any ".sidebar-link" (event delegation)
document.addEventListener('click', (e) => {
  // Find the nearest ancestor with our class + data-value (works if you click the icon/text)
  const a = e.target.closest('.sidebar-link[data-value]');
  if (!a) return;                     // Ignore clicks outside the menu
  e.preventDefault();                 // Prevent jumping to "#" (top of page)

  // 2) Visual highlight: remove 'active' from all, add to the clicked one
  document.querySelectorAll('.sidebar-link').forEach((el) => {
    el.classList.remove('active');
    el.setAttribute('aria-current', 'false');  // a11y: not current
  });
  a.classList.add('active');
  a.setAttribute('aria-current', 'page');      // a11y: this link represents the current page

  // 3) Tell Shiny which page to show (matches nav_panel(value = "..."))
  Shiny.setInputValue('sidebar_select', a.dataset.value, { priority: 'event' });
});

// 4) Keep the highlight in sync when the server changes the page programmatically
Shiny.addCustomMessageHandler('setActiveSidebar', (value) => {
  document.querySelectorAll('.sidebar-link').forEach((el) => {
    const active = el.dataset.value === value;
    el.classList.toggle('active', active);
    el.setAttribute('aria-current', active ? 'page' : 'false');
  });
});
