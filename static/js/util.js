export const redirectToLogin = () => {
  const returnUrl = encodeURIComponent(window.location.pathname);
  window.location.href = `/admin/login?from=${returnUrl}`;
};

export const base64URLDecode = v => {
  let b64 = v.replace(/-/g, '+').replace(/_/g, '/');
  if (b64.length % 4 !== 0) {
    b64 = b64.padEnd(b64.length + (4 - b64.length % 4), '=');
  }
  return window.atob(b64);
}
