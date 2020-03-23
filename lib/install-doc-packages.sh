#/bin/sh
# install-doc-packages.sh --- find and install ‘-doc’ packages related to installed packages

preferred_lang='en'

tmpfil="$(mktemp --tmpdir install-doc-packagesXXXXXXXXXXXXXXXXXXX)"

qfmt='${db:Status-Abbrev}${binary:Package}\n'

# This works as such:
#
# 1. Find all selections marked for installation (dpkg + awk)
# 2. Append ‘-doc’ to all packages, removing arch tags (e.g. :amd64) if necessary (sed)
# 3. Find those from above that exist and marked as not-installed (xargs dpkg-query + awk)
dpkg --get-selections                             \
     | awk '/\<install$/ { print $1 }'            \
     | sed -E 's,(:[[:alnum:]]+)?$,-doc,'         \
     | xargs dpkg-query -f "$qfmt" -W 2>/dev/null \
     | awk '/^un/ { print $2 }'                   \
     |                                            \
# 4. For each package....
     while read pkg;
     do
# 4.1. ... determine if it is virtual.

         # XXX: Horrid hack to test if $pkg is virtual.  apt-cache(8)
         # prints nothing to stdout if the package is virtual.  The same is
         # true also when the package does not exists, but we’re supposed
         # to be sure that it exists at this point.
         if apt-cache show "$pkg" 2>/dev/null | grep '' >/dev/null;
         then                   # concrete

# 4.1.1. If it is not, then print the package name as it is, ...
             echo "$pkg"
         else                   # virtual

# 4.1.2. ... if it is not virtual, try <packagename>-${preferred_lang}.
             prefd="$pkg-$preferred_lang"
             if dpkg-query -W "$prefd" 2>/dev/null 1>&2;
             then

# 4.1.2.1. If that works, print the new package name
                 echo "$prefd"
             else

# 4.1.2.2. ... otherwise, print package name to stderr for later collection.
                 echo "$pkg" 1>&2
             fi
         fi

# 4.2. Redirect stderr to a temporary file for a later report, and use
#      stdout as the argument list for ‘apt-get(8)’ via ‘xargs(1)’.
#      The ‘-o’ flag allows apt-get to read from TTY, so that its
#      interactive features can be used for a final confirmation.
     done 2>$tmpfil \
         | xargs -o apt-get install --install-recommends --install-suggests

# 5. Collect the exit code from the monster of a pipeline above.
xit=$?

# 6. If the file for packages we couldn’t install (i.e. the stderr
#    from the while loop in (4), print out its contents and set exit
#    code to ‘2’ to indicate incomplete installation.
if [ -s "$tmpfil" ]; then
    echo
    echo "The following packages were not installed because no"
    echo "concrete alternatives were found":
    fmt --goal=68 $tmpfil | sed 's/^/    /'
    xit=2
fi

# 7. Cleanup and exit.
rm $tmpfil
exit $xit
