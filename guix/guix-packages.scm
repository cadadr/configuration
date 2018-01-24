;; My Guix packages.

(define-module (guix-packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)

  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tls))

(define-public emacs-git
  (package (inherit emacs)
    (name "emacs-git")
    (version "local-master")
    (synopsis "Emacs build from local git copy")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		     (url "file:///tmp/emacs-co")
		     (commit "288b3ca2e519903653b9bc88d281ecd9f6b162a4")))
	      (sha256
	       (base32 "00y17rip4xca4510jcimn1lxrgdzlmcbxshbjdnj6mf3hw1bpxpq"))))
    (arguments (append '(#:configure-flags
			 '("--with-mailutils" "--with-file-notification=yes"
			   "--with-modules"))
		       (package-arguments emacs)))))

;; (define-public vtwm
;;   (package
;;     (name "vtwm")
;;     (version "TODO")
;;     (source (origin
;; 	      (method url-fetch)
;; 	      (uri (string-append "TODO" version ".tar.gz"))
;; 	      (sha256
;;                (base32 "TODO"))))
;;     (build-system TODO)
;;     (arguments '(#:configure-flags '("TODO")))
;;     (inputs TODO)
;;     (synopsis "TODO")
;;     (home-page "TODO")
;;     (license TODO)))

;;; guix packages nonfree - package definitions for GNU Guix
;;; Copyright (C) 2017 ng0
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Adopted from https://wingolog.org/pub/linux-nonfree.scm with
;;; further changes and updates. Original code license below.

;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

;; From https://gitlab.com/ng0_guix/guix_packages_nonfree/raw/master/crash/burn/linux-nonfree.scm

(define (linux-nonfree-urls version)
  "Return a list of URLs for linux-nonfree VERSION."
  (list (string-append
         "https://www.kernel.org/pub/linux/kernel/v4.x/"
         "linux-" version ".tar.xz")))

(define-public linux-nonfree
  (let* ((version "4.11.7"))
    (package
      (inherit linux-libre)
      (name "linux-nonfree")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (linux-nonfree-urls version))
         (sha256
          (base32
           "1v94lkc4lzb2k9c3y7j6h4zx827abcy7jqm9jlp5n15dlbmj6bsk"))))
      (native-inputs
       `(("openssl" ,openssl)
         ,@(package-native-inputs linux-libre)))
      (synopsis "Mainline Linux kernel.")
      (description "Nonfree Linux kernel.")
      (license license:gpl2)
      (home-page "http://kernel.org"))))

(define-public perf-nonfree
  (package
    (inherit perf)
    (name "perf-nonfree")
    (version (package-version linux-nonfree))
    (source (package-source linux-nonfree))
    (license (package-license linux-nonfree))))
