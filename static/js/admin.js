import { createPost, regeneratePosts, regenerateImagePreviews } from './api.js'
import { redirectToLogin } from './util.js'
import { startJobMonitoring, handleCancelJob, handleCloseModal } from './job.js'

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

const regeneratePreviewsHandler = async e => {
  const btn = e.target;
  e.preventDefault();

  // Prevent double-clicks
  if (btn.disabled) return;

  btn.disabled = true;
  const originalText = btn.textContent;
  btn.textContent = 'Starting...';

  try {
    const jobId = await regenerateImagePreviews();
    btn.textContent = originalText;
    startJobMonitoring('Images preview regeneration', jobId);
  } catch (err) {
    // Re-enable button on error
    btn.disabled = false;
    btn.textContent = originalText;

    if (err.code === 401) {
      redirectToLogin();
    } else if (err.code === 409) {
      alert('Preview regeneration is already in progress');
    } else {
      alert(`Error: ${err.message}`);
    }
  }
};

const onLoad = () => {
  const newPostBtn = document.getElementById('new-post-btn');
  const regenerateBtn = document.getElementById('regenerate-all-btn');
  const regeneratePreviewsBtn = document.getElementById('regenerate-previews-btn');
  const jobCancelBtn = document.getElementById('job-cancel-btn');
  const jobCloseBtn = document.getElementById('job-close-btn');

  if (newPostBtn) {
    newPostBtn.addEventListener('click', newPostHandler);
  }

  if (regenerateBtn) {
    regenerateBtn.addEventListener('click', regenerateHandler);
  }

  if (regeneratePreviewsBtn) {
    regeneratePreviewsBtn.addEventListener('click', regeneratePreviewsHandler);
  }

  if (jobCancelBtn) {
    jobCancelBtn.addEventListener('click', handleCancelJob);
  }

  if (jobCloseBtn) {
    jobCloseBtn.addEventListener('click', handleCloseModal);
  }
};

document.addEventListener('DOMContentLoaded', onLoad);
