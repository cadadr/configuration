# loctz.sh --- Locale and timezone.

MM_CHARSET=UTF-8;		export MM_CHARSET
LANG=en_GB.$MM_CHARSET;		export LANG
LANGUAGE=$LANG;			export LANGUAGE
if [ -d /usr/share/locale/tr/ ]; then
    LC_MONETARY=tr_TR.$MM_CHARSET;	export LC_MONETARY
    LC_TIME=tr_TR.$MM_CHARSET;		export LC_TIME
fi

# Timezone
TZ=Europe/Istanbul;	export TZ
