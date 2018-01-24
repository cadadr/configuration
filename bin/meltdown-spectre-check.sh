#!/bin/sh

ARCHKCONFIG=/proc/config.gz
UBUNTUKCONFIG=/boot/config-$(uname -r)

(
    if [ -f $ARCHKCONFIG ]; then
	## arch
	zgrep CONFIG_PAGE_TABLE_ISOLATION $ARCHKCONFIG >/dev/null || exit 1
    elif [ -f $UBUNTUKCONFIG ]; then
	grep CONFIG_PAGE_TABLE_ISOLATION $UBUNTUKCONFIG >/dev/null || exit 1
    fi

    dmesg | grep isolation >/dev/null || exit 1
) \
    || \
    echo "This computer with current kernel ($(uname -r)) is vulnerable."
