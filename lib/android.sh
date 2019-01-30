# android.sh --- Android development environment settings.

### Environment:
optdir="$HOME/opt"

export ANDROID_HOME="$optdir/android-sdk-tools"
export ANDROID_PTOOLS="$ANDROID_HOME/platform-tools"
export FLUTTER="$optdir/flutter"

PATH="$PATH:$ANDROID_HOME/tools/bin:$FLUTTER/bin"

### Helpers:
alias terriblepileofshit="$optdir/android-studio/bin/studio.sh"
alias ptool="printf \"$ANDROID_PTOOLS/%s\""
alias flutterdocs="$FLUTTER/dev/bots/docs.sh"
