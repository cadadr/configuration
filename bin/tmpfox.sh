#!/bin/sh
# tmpfox.sh --- start firefox w/ empty profile, but copy setup from the default

# From: https://news.ycombinator.com/item?id=18899291

PROFILEDIR=`mktemp -p /tmp -d tmp-fx-profile.XXXXXX.d`
mkdir -p $PROFILEDIR/extensions
cd ~/.mozilla/firefox/*.default/
cp extensions/* $PROFILEDIR/extensions/
cp addons.json extensions.json $PROFILEDIR/
cp -R browser-extension-data $PROFILEDIR/
cp cert_override.txt $PROFILEDIR/
cp prefs.js $PROFILEDIR/
echo 'user_pref("devtools.selfxss.count", 10);' > $PROFILEDIR/prefs.js
cd -
firefox -profile $PROFILEDIR -no-remote -new-instance
rm -rf $PROFILEDIR
