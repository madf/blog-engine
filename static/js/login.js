const submitHandler = async e => {
  e.preventDefault();

  const submitBtn = document.getElementById('submit-btn');
  const errorDiv = document.getElementById('error-message');

  submitBtn.disabled = true;
  submitBtn.textContent = 'Logging in...';
  errorDiv.classList.remove('visible');

  try {
    const formData = new FormData(e.target);
    const response = await fetch('/admin/api/token/issue', {
      method: 'POST',
      body: formData
    });

    if (response.ok) {
      const params = new URLSearchParams(window.location.search);
      const from = params.get('from') || '/admin';
      window.location.href = from;
    } else {
      const error = await response.json();
      errorDiv.textContent = error || 'Login failed';
      errorDiv.classList.add('visible');
      submitBtn.disabled = false;
      submitBtn.textContent = 'Login';
    }
  } catch (err) {
    errorDiv.textContent = 'Network error. Please try again.';
    errorDiv.classList.add('visible');
    submitBtn.disabled = false;
    submitBtn.textContent = 'Login';
  }
};

document.getElementById('login-form').addEventListener('submit', submitHandler);
