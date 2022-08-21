# Installing Qutebrowser

If needed, a recent version of Qutebrowser can be installed via

    $ cd ~/Sources/External/github-qutebrowser-qutebrowser     # where a checkout usually
                                                               # is on my system
    $ python3 ./scripts/mkvenv.py --venv-dir ~/local/_qutebrowser
    $ cd -
    $ ~/local/_qutebrowser/bin/pip install readability-lxml

from inside a recent checkout.  The underscore is there in the path
name in order to tell `lib/profile/paths.sh` to skip adding this three
to the path, as otherwise it will shadow system’s Python 3
installation.

Qutebrowser is going to fail to keep login sessions because different
versions of WebEngine databases are incompatible
(viz. <https://github.com/qutebrowser/qutebrowser/issues/5847#issuecomment-718985559>),
so we need to delete the old database if it exists:

    $ rm -r $HOME/.local/share/qutebrowser/webengine

(`bin/launch-qutebrowser.bash` should deal with this automatically, in case
you followed the above steps to build and install Qutebrowser, instead of
installing it through a package manager.)
