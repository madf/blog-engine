import { createPost, regeneratePosts } from './api.js'
import { redirectToLogin } from './util.js'

const newPostHandler = async e => {
  const newPostBtn = e.target;
  e.preventDefault();

  newPostBtn.disabled = true;
  newPostBtn.textContent = 'Creating...';

  try {
    const post = await createPost();
    window.location.href = `/admin/posts/${post.slug}/edit`;
  } catch (err) {
    if (err.code === 401) {
      redirectToLogin();
    } else {
      alert(`Error: ${err.message}`);
      newPostBtn.disabled = false;
      newPostBtn.textContent = 'New Post';
    }
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
    await regeneratePosts();
    regenerateBtn.textContent = 'Done!';
    setTimeout(() => {
      regenerateBtn.disabled = false;
      regenerateBtn.textContent = 'Regenerate All Pages';
    }, 2000);
  } catch(err) {
    if (err.code === 401) {
      redirectToLogin();
    } else {
      alert(`Error: ${err.message}`);
      regenerateBtn.disabled = false;
      regenerateBtn.textContent = 'Regenerate All Pages';
    }
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
