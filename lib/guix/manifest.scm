;;; manifest.scm --- packages to be installed for my profile, some customised

(use-package-modules julia haskell haskell-apps)

(packages->manifest
 (list
  glibc-locales glibc-utf8-locales

  julia

  ghc cabal-install))
