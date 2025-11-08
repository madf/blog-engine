const getAuthToken = () => {
  const match = document.cookie.match(/authtoken=([^;]+)/);
  return match ? match[1] : null;
};

export const authFetch = async (url, options = {}) => {
  const token = getAuthToken();
  if (!token) {
    throw new Error('No authentication token found');
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
  if (response.ok) {
    return await response.json();
  } else {
    return null;
  }
};

export const regeneratePosts = async () => {
  const response = await authFetch('/admin/api/posts/regenerate', { method: 'POST' });
  return response.ok;
};

export const updatePost = async (slug, data) => {
  const response = await authFetch(`/admin/api/posts/${slug}`, {
    method: 'PUT',
    body: data
  });

  if (!response.ok) {
    throw new Error(`Upload failed: ${response.statusText}`);
  }
};

export const uploadImage = async (slug, data) => {
  const response = await authFetch(`/admin/api/posts/${slug}/image`, {
    method: 'POST',
    body: data
  });

  if (!response.ok) {
    throw new Error(`Upload failed: ${response.statusText}`);
  }

  return await response.json();
};

export const updateImageCaption = async (id, data) => {
  const response = await authFetch(`/admin/api/image/${id}`, {
    method: 'PUT',
    body: data
  });

  if (!response.ok) {
    throw new Error(`Upload failed: ${response.statusText}`);
  }
};

export const deleteImage = async id => {
  const response = await authFetch(`/admin/api/image/${id}`, {
    method: 'DELETE'
  });
  if (!response.ok) {
    throw new Error(`Failed to delete image: ${response.statusText}`);
  }
};
