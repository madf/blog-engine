import { getJobStatus, cancelJob } from './api.js';
import { showModal, hideModal } from './modal.js';
import { redirectToLogin } from './util.js';

let currentJobId = null;
let pollInterval = null;

const MODAL_ID = 'job-modal';

export const startJobMonitoring = (jobName, jobId) => {
  currentJobId = jobId;

  const title = document.getElementById('job-modal-title');
  const progress = document.getElementById('job-progress');
  const status = document.getElementById('job-status');
  const cancelBtn = document.getElementById('job-cancel-btn');

  title.textContent = jobName;
  progress.style.width = '0%';
  progress.textContent = '0%';
  status.textContent = 'Starting...';
  cancelBtn.disabled = false;

  showModal(MODAL_ID);
  startPolling();
};

export const stopJobMonitoring = () => {
  hideModal(MODAL_ID);
  stopPolling();
  currentJobId = null;
};

const formatDuration = (seconds) => {
  if (seconds < 60) {
    return `${Math.round(seconds)}s`;
  } else if (seconds < 3600) {
    const mins = Math.floor(seconds / 60);
    const secs = Math.round(seconds % 60);
    return secs > 0 ? `${mins}m ${secs}s` : `${mins}m`;
  } else {
    const hours = Math.floor(seconds / 3600);
    const mins = Math.round((seconds % 3600) / 60);
    return mins > 0 ? `${hours}h ${mins}m` : `${hours}h`;
  }
};

const calculateETA = (createdAt, progress) => {
  if (!createdAt || !progress || progress === 0) return null;

  const startTime = new Date(createdAt);
  const now = new Date();
  const elapsedMs = now - startTime;
  const elapsedSeconds = elapsedMs / 1000;

  const timePerPercent = elapsedSeconds / progress;

  const remainingPercent = 100 - progress;
  const remainingSeconds = timePerPercent * remainingPercent;

  return remainingSeconds;
};

const formatResult = (result) => {
  if (!result) return '';

  if (result.num_images !== undefined) {
    const lines = [`Processed ${result.num_images} images in ${result.duration.toFixed(2)}s`];
    if (result.num_failures > 0) {
      lines.push(`${result.num_failures} failures`);
    }
    return lines.join(', ');
  }

  return '';
};

const updateProgress = (jobStatus) => {
  const progress = document.getElementById('job-progress');
  const status = document.getElementById('job-status');
  const cancelBtn = document.getElementById('job-cancel-btn');

  const state = jobStatus.state;

  if (state.status === 'queued') {
    progress.style.width = '0%';
    progress.textContent = '0%';
    status.textContent = 'Queued...';
  } else if (state.status === 'running') {
    const percent = state.progress || 0;
    progress.style.width = `${percent}%`;
    progress.textContent = `${percent}%`;

    // Calculate and display ETA
    const eta = calculateETA(jobStatus.created, percent);
    if (eta && eta > 1) {
      status.textContent = `Running... (ETA: ${formatDuration(eta)})`;
    } else {
      status.textContent = 'Running...';
    }
  } else if (state.status === 'completed') {
    progress.style.width = '100%';
    progress.textContent = '100%';
    const resultText = formatResult(state.result);
    status.textContent = resultText ? `Completed: ${resultText}` : 'Completed successfully!';
    cancelBtn.disabled = true;
    stopPolling();
    // Clean up completed job from registry
    cleanupJob();
  } else if (state.status === 'failed') {
    status.textContent = `Failed: ${state.error || 'Unknown error'}`;
    cancelBtn.disabled = true;
    stopPolling();
    // Clean up failed job from registry
    cleanupJob();
  }
};

const cleanupJob = async () => {
  if (currentJobId === null) return;

  try {
    await cancelJob(currentJobId);
  } catch (err) {
    console.log('Job cleanup:', err.message);
  }
};

const startPolling = () => {
  if (pollInterval) {
    clearInterval(pollInterval);
  }

  pollInterval = setInterval(async () => {
    if (currentJobId === null) {
      stopPolling();
      return;
    }

    try {
      const jobStatus = await getJobStatus(currentJobId);
      updateProgress(jobStatus);
    } catch (err) {
      if (err.code === 401) {
        stopPolling();
        stopJobMonitoring();
        redirectToLogin();
      } else if (err.code === 404) {
        stopPolling();
        const status = document.getElementById('job-status');
        const cancelBtn = document.getElementById('job-cancel-btn');
        status.textContent = 'Job no longer exists (completed or cancelled)';
        cancelBtn.disabled = true;
      } else {
        console.error('Error polling job status:', err);
      }
    }
  }, 500);
};

const stopPolling = () => {
  if (pollInterval) {
    clearInterval(pollInterval);
    pollInterval = null;
  }
};

export const handleCancelJob = async () => {
  if (currentJobId === null) return;

  try {
    await cancelJob(currentJobId);
    stopJobMonitoring();
  } catch (err) {
    if (err.code === 401) {
      redirectToLogin();
    } else {
      alert(`Error canceling job: ${err.message}`);
    }
  }
};

export const handleCloseModal = () => {
  stopJobMonitoring();
  // Re-enable the regenerate button
  const btn = document.getElementById('regenerate-previews-btn');
  if (btn) {
    btn.disabled = false;
  }
};
