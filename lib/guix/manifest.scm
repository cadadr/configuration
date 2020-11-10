;;; manifest.scm --- packages to be installed for my profile, some customised

(use-package-modules mail web-browsers)

(packages->manifest
 (list
  ;; Guix suggests these
  glibc-locales glibc-utf8-locales

  ;; my packages
  mu qutebrowser))
