# Name of your emacs binary
EMACS	= emacs
prefix	= $(HOME)/co/emacs.d

# Where local lisp files go.
lispdir= $(prefix)/packages/org

# Where local data files go.
datadir = $(prefix)/extras/org

# Where info files go.
infodir = $(prefix)/docs

ORG_MAKE_DOC = info # html pdf

ORG_ADD_CONTRIB = org-* ox-* ob-*

# Where to create temporary files for the testsuite
# respect TMPDIR if it is already defined in the environment
TMPDIR ?= /tmp
testdir = $(TMPDIR)/tmp-orgtest

# Configuration for testing
# add options before standard load-path
BTEST_PRE   =
# add options after standard load path
BTEST_POST  =
              # -L <path-to>/ert      # needed for Emacs23, Emacs24 has ert built in
              # -L <path-to>/ess      # needed for running R tests
              # -L <path-to>/htmlize  # need at least version 1.34 for source code formatting
BTEST_OB_LANGUAGES = awk C fortran maxima lilypond octave python sh perl
              # R                     # requires ESS to be installed and configured
# extra packages to require for testing
BTEST_EXTRA =
              # ess-site  # load ESS for R tests
# See default.mk for further configuration options.
