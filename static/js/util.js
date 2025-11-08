export const redirectToLogin = () => {
  const returnUrl = encodeURIComponent(window.location.pathname);
  window.location.href = `/admin/login?from=${returnUrl}`;
};
