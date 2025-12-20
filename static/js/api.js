class ApiError extends Error {
  constructor(message, code, details) {
    super(message);
    this.name = 'ApiError';
    this.code = code;
    this.details = details;
  }

  isAuthError() {
    return this.code === 401 || this.code === 403;
  }
}

const getAuthToken = () => {
  const match = document.cookie.match(/authtoken=([^;]+)/);
  return match ? match[1] : null;
};

export const authFetch = async (url, options = {}) => {
  const token = getAuthToken();
  if (!token) {
    throw new ApiError('No authentication token found', 401, 'Unauthorized');
  }

  const headers = options.headers || {};
  // CSRF mitigation
  headers['Authorization'] = `Bearer ${token}`;

  return await fetch(url, {
    ...options,
    headers
  });
};

export const createPost = async () => {
  const response = await authFetch('/admin/api/posts', { method: 'POST' });

  if (!response.ok) {
    throw new ApiError('Failed to create post', response.status, response.statusText);
  }

  return await response.json();
};

export const regeneratePosts = async () => {
  const response = await authFetch('/admin/api/posts/regenerate', { method: 'POST' });

  if (!response.ok) {
    throw new ApiError('Failed to regenerate posts', response.status, response.statusText);
  }
};

export const updatePost = async (slug, data) => {
  const response = await authFetch(`/admin/api/posts/${slug}`, {
    method: 'PUT',
    body: data
  });

  if (!response.ok) {
    throw new ApiError('Failed to update post', response.status, response.statusText);
  }
};

export const uploadImage = async (slug, data) => {
  const response = await authFetch(`/admin/api/posts/${slug}/image`, {
    method: 'POST',
    body: data
  });

  if (!response.ok) {
    throw new ApiError('Failed to upload image', response.status, response.statusText);
  }

  return await response.json();
};

export const deleteImage = async id => {
  const response = await authFetch(`/admin/api/image/${id}`, {
    method: 'DELETE'
  });

  if (!response.ok) {
    throw new ApiError('Failed to delete image', response.status, response.statusText);
  }
};
