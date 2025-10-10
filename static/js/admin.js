document.addEventListener('DOMContentLoaded', function() {
  const regenerateBtn = document.getElementById('regenerate-all-btn');

  if (regenerateBtn) {
    regenerateBtn.addEventListener('click', function() {
      if (!confirm('This will regenerate all static pages. Continue?')) return;

      this.disabled = true;
      this.textContent = 'Regenerating...';

      fetch('/admin/api/post/regenerate', { method: 'POST' })
        .then(response => {
          if (response.ok) {
            this.textContent = 'Done!';
            setTimeout(() => {
              this.disabled = false;
              this.textContent = 'Regenerate All Pages';
            }, 2000);
          } else {
            alert('Failed to regenerate pages');
            this.disabled = false;
            this.textContent = 'Regenerate All Pages';
          }
        })
        .catch(err => {
          alert('Error: ' + err);
          this.disabled = false;
          this.textContent = 'Regenerate All Pages';
        });
    });
  }
});
