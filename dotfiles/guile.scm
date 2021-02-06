;; guile.scm -- Customise Guile interactive session. -*- mode: scheme -*-

(use-modules (ice-9 readline))
(activate-readline)
(add-to-load-path ".")

(cond ((false-if-exception (resolve-interface '(ice-9 readline)))
       =>
       (lambda (module)
         ;; Enable completion and input history at the REPL.
         ((module-ref module 'activate-readline))))
      (else
       (display "Consider installing the 'guile-readline' package for
convenient interactive line editing and input history.\n\n")))

      (unless (getenv "INSIDE_EMACS")
        (cond ((false-if-exception (resolve-interface '(ice-9 colorized)))
               =>
               (lambda (module)
                 ;; Enable completion and input history at the REPL.
                 ((module-ref module 'activate-colorized))))
              (else
               (display "Consider installing the 'guile-colorized' package
for a colorful Guile experience.\n\n"))))
