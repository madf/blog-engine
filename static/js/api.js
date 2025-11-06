const getAuthToken = () => {
  const match = document.cookie.match(/authtoken=([^;]+)/);
  return match ? match[1] : null;
}

export const authFetch = (url, options = {}) => {
  const token = getAuthToken();
  if (!token) {
    throw new Error('No authentication token found');
  }

  const headers = options.headers || {};
  // CSRF mitigation
  headers['Authorization'] = `Bearer ${token}`;

  return fetch(url, {
    ...options,
    headers
  });
}
