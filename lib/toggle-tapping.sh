# toggle-tapping.sh --- toggle tap-to-click in touchpad
. $MYLIB/fns.sh

propidRe='s/.*\(([0-9]+)\).*/\1/'
propvalRe='s/.*(0|1)$/\1/'

touchpad="$(xinput --list --name-only | grep -i touchpad)"

tapping="$(xinput list-props "$touchpad" | grep -i 'tapping enabled (')"

tappingToggle="$(echo $tapping | sed -E $propidRe)"
tappingValue="$(echo $tapping | sed -E $propvalRe)"

newValue="$(( ! $tappingValue ))"

xinput set-prop "$touchpad" "$tappingToggle" "$newValue"

# Check results
newTapping="$(xinput list-props "$touchpad" | grep -i 'tapping enabled (')"
result="$(echo $newTapping | sed -E $propvalRe)"

export GK_NOTIFY=yes

if [ "$result" -eq 1 ]; then
    say Tapping enabled
else
    say Tapping disabled
fi
