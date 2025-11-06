import { authFetch } from './api.js'

document.addEventListener('DOMContentLoaded', () => {
  const newPostBtn = document.getElementById('new-post-btn');
  const regenerateBtn = document.getElementById('regenerate-all-btn');

  if (newPostBtn) {
    newPostBtn.addEventListener('click', async () => {
      newPostBtn.disabled = true;
      newPostBtn.textContent = 'Creating...';

      try {
        const response = await authFetch('/admin/api/posts', { method: 'POST' });

        if (response.ok) {
          const post = await response.json();
          window.location.href = `/admin/posts/${post.slug}/edit`;
        } else {
          alert('Failed to create post');
          newPostBtn.disabled = false;
          newPostBtn.textContent = 'New Post';
        }
      } catch (err) {
        alert('Error: ' + err);
        newPostBtn.disabled = false;
        newPostBtn.textContent = 'New Post';
      }
    });
  }

  if (regenerateBtn) {
    regenerateBtn.addEventListener('click', function() {
      if (!confirm('This will regenerate all static pages. Continue?')) return;

      this.disabled = true;
      this.textContent = 'Regenerating...';

      authFetch('/admin/api/post/regenerate', { method: 'POST' })
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
