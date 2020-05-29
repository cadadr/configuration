;;; manifest.scm --- packages to be installed for my profile, some customised

(use-package-modules julia haskell haskell-apps mail backup)

(packages->manifest
 (list
  glibc-locales glibc-utf8-locales

  julia

  borg mu))
