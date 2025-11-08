import { createPost, regeneratePosts } from './api.js'

const newPostHandler = async e => {
  const newPostBtn = e.target;
  e.preventDefault();

  newPostBtn.disabled = true;
  newPostBtn.textContent = 'Creating...';

  try {
    const post = await createPost();

    if (post) {
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
};

const regenerateHandler = async e => {
  const regenerateBtn = e.target;
  e.preventDefault();

  if (!confirm('This will regenerate all static pages. Continue?')) {
    return;
  }

  regenerateBtn.disabled = true;
  regenerateBtn.textContent = 'Regenerating...';

  try {
    const r = await regeneratePosts();

    if (r) {
      regenerateBtn.textContent = 'Done!';
      setTimeout(() => {
        regenerateBtn.disabled = false;
        regenerateBtn.textContent = 'Regenerate All Pages';
      }, 2000);
    } else {
      alert('Failed to regenerate pages');
      regenerateBtn.disabled = false;
      regenerateBtn.textContent = 'Regenerate All Pages';
    }
  } catch(err) {
      alert('Error: ' + err);
      regenerateBtn.disabled = false;
      regenerateBtn.textContent = 'Regenerate All Pages';
  }
};

const onLoad = () => {
  const newPostBtn = document.getElementById('new-post-btn');
  const regenerateBtn = document.getElementById('regenerate-all-btn');

  if (newPostBtn) {
    newPostBtn.addEventListener('click', newPostHandler);
  }

  if (regenerateBtn) {
    regenerateBtn.addEventListener('click', regenerateHandler);
  }
};

document.addEventListener('DOMContentLoaded', onLoad);
