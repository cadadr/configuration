;; Load swank and dump a core image to load slime faster.
(load (truename #p"~/co/emacs.d/packages/slime/swank-loader.lisp"))
(swank-loader:dump-image #p"~/co/emacs.d/extras/slime.fasl")
