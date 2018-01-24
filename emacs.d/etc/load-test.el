;; load-test.el -- Test script for configuration.
(let
    ((time-start (current-time)))
  (message
   "Startup took %f seconds..."
   (car (benchmark-run 1
          (progn
            (load (expand-file-name "~/.emacs"))
            (message "-OK-"))))))
