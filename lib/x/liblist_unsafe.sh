#!/bin/sh
#
# The MIT License (MIT)
#
# Copyright (c) 2017-2018 Thomas "Ventto" Venriès <thomas.venries@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

##
# @brief    Prints a list from arguments
# @usage    list <elt> <elt> ...
# @print    The list result
#
list () {
    for e; do [ -n "$e" ] && echo "$e"; done
}

##
# @brief    Prints the number of elements in the list
# @usage    list_size <lst>
# @print    The size of the list as positive integer
#
list_size () {
    test "$#" -ne 1 && { echo "0"; return; }
    eval "test -n \"\$$1\"" && { eval "echo \"\$$1\" | wc -l"; return; }
    echo '0'
}

##
# @brief    Returns whether the list is empty(1) or not(0)
# @usage    list_empty <lst>
# @return   0 or 1
#
list_empty () {
    eval "test -z \"\$$1\""
}

##
# @brief    Adds a new element at the beginning of the list
# @usage    list_push_front <lst> <elt>
# @out      Set the variable passed by reference
#
list_push_front () {
    test "$#" -ne 2 && return 1
    eval "test -n \"\$$1\"" || { eval "$1='$2'"; return; }
    eval "$1=\"\$(printf '%s\n%s' '$2' \"\$$1\")\"";
}

##
# @brief    Adds a new element at the end of the list
# @usage    list_push_back <lst> <elt>
# @out      Set the variable passed by reference
#
list_push_back () {
    test "$#" -ne 2 && return 1
    eval "test -n \"\$$1\"" || { eval "$1='$2'"; return; }
    eval "$1=\"\$(printf '%s\n%s' \"\$$1\" '$2')\"";
}

##
# @brief    Inserts new elements in the list before a specified position
# @usage    list_insert <lst> <elt> <index>
# @out      Set the variable passed by reference
#
list_insert () {
    test "$#" -ne 3 && return 1; i="$3"; [ "$i" != '$' ] &&  i=$((i+1))
    eval "$1=\"\$(echo \"\$$1\" | sed \"${i}i${2}\")\""
}

##
# @brief    Modifies an element from the list at a specified position
# @usage    list_set <lst> <elt> <index>
# @out      Set the variable passed by reference
#
list_set () {
    test "$#" -ne 3 && return 1
    i="$3"; i=$((i+1)); eval "$1=\"\$(echo \"\$$1\" | sed \"${i}s/.*/$2/\")\""
}

##
# @brief    Extracts a range of elements from the list between two specified
#           positions
# @usage    list_extract <lst> <from_index> <to_index>
# @out      Set the variable passed by reference
#
list_extract () {
    test "$#" -ne 3 && return 1; i="$2"; j="$3"; i=$((i+1)); j=$((j+1))
    eval "$1=\"\$(echo \"\$$1\" | sed -n \"${i},${j}p\")\""
}

##
# @brief    Replaces all elements from the list with a specified element
# @usage    list_replace <lst> <old_elt> <new_elt>
# @out      Set the variable passed by reference
#
list_replace () {
    test "$#" -ne 3 && return 1
    eval "$1=\"\$(echo \"\$$1\" | sed -e \"s/^$2$/$3/g\")\""
}

##
# @brief    Prints the element at a specified position
# @usage    list_get <lst> <index>
# @print    The element found
#
list_get () {
    test "$#" -ne 2 && return 1; i="$2"; i=$((i+1));
    eval "echo \"\$$1\" | sed -n \"${i}p\""
}

##
# @brief    Prints the head of the list
# @usage    list_front <lst>
# @print    The element found
#
list_front () {
    test "$#" -ne 1 && return 1; eval "echo \"\$$1\" | sed -n '1p'"
}

##
# @brief    Prints the queue of the list
# @usage    list_back <lst>
# @print    The element found
#
list_back () {
    test "$#" -ne 1 && return 1; eval "echo \"\$$1\" | sed -n '\$p'"
}

##
# @brief    Removes the first-hit element from a list
# @usage    list_erase <lst> <elt>
# @out      Set the variable passed by reference
#
list_erase () {
    test "$#" -ne 2 && return 1
    eval "$1=\"\$(echo \"\$$1\" | sed -e \"0,/^$2$/ s///\" -e '/^$/d')\""
}

##
# @brief    Removes a range of elements from a list between two specified
#           positions
# @usage    list_erase_range <lst> <from_index> <to_index>
# @out      Set the variable passed by reference
#
list_erase_range () {
    test "$#" -ne 3 && return 1
    i="$2"; j="$3"; i=$((i+1)); j=$((j+1));
    eval "$1=\"\$(echo \"\$$1\" | sed \"${i},${j}d\")\""
}

##
# @brief    Removes all elements from a specified position
# @usage    list_erase_from <lst> <index>
# @out      Set the variable passed by reference
#
list_erase_from () {
    test "$#" -ne 2 && return 1
    i="$2"; i=$((i+1)); eval "$1=\"\$(echo \"\$$1\" | sed \"${i},\\\$d\")\""
}

##
# @brief    Removes the element at a specified position
# @usage    list_eraseat <lst> <index>
# @out      Set the variable passed by reference
#
list_eraseat () {
    test "$#" -ne 2 && return 1
    i="$2"; i=$((i+1)); eval "$1=\"\$(echo \"\$$1\" | sed \"${i}d\")\""
}

##
# @brief    Removes all the elements from the list, which are equal to given
#           element
# @usage    list_remove <lst> <elt>
# @out      Set the variable passed by reference
#
list_remove () {
    test "$#" -ne 2 && return 1
    eval "$1=\"\$(echo \"\$$1\" | sed -e \"/^$2$/d\")\""
}

##
# @brief    Removes the first element of the list
# @usage    list_pop_front <lst>
# @out      Set the variable passed by reference
#
list_pop_front () {
    test "$#" -ne 1 && return 1; eval "$1=\"\$(echo \"\$$1\" | sed '1d')\""
}

##
# @brief    Removes the last element of the list
# @usage    list_pop_back <lst>
# @out      Set the variable passed by reference
#
list_pop_back () {
    test "$#" -ne 1 && return 1; eval "$1=\"\$(echo \"\$$1\" | sed '\$d')\""
}

##
# @brief    Prints the index of a specified element
# @usage    list_indexof <lst> <elt>
# @print    positive integer or -1 if not found
#
list_indexof () {
    test "$#" -ne 2 && return 1; i=0
    eval "for e in \$$1; do
              [ \"\$e\" = '$2' ] && { echo \"\$i\"; return 0; }; i=\$((i+1));
          done"
    return 1
}

##
# @brief    Returns whether the list contains a specified element(0) or not(1)
# @usage    list_contains <lst> <elt>
# @return   0 or 1
#
list_contains () {
    test "$#" -ne 2 && return 1
    eval "for e in \$$1; do [ \"\$e\" = '$2' ] && return 0; done; return 1"
}

##
# @brief    Prints the number of a specified element in the list
# @usage    list_count <lst> <elt>
# @print    The number of elements as positive integer
#
list_count () {
    test "$#" -ne 2 && { echo '0'; return; }
    eval "i=0; for e in \$$1; do [ \"\$e\" = '$2' ] && { i=\$((i+1)); };done;"
    echo "$i"
}

##
# @brief    Maps every element of the list
# @usage    list_maps <lst> <func>
# @out      Set the variable passed by reference
#
list_map () {
    test "$#" -ne 2 && return 1
    eval "$1=\"\$(for e in \$$1; do eval \"$2 \$e\"; done)\""
}

##
# @brief    Reverses the list
# @usage    list_reverse <lst>
# @out      Set the variable passed by reference
#
list_reverse() {
    test "$#" -ne 1 && return 1
    eval "$1=\"\$(echo \"\$$1\" | sed '1!x;H;1h;\$!d;g')\""
}

##
# @brief    Sorts the list
# @usage    list_sort <lst>
# @out      Set the variable passed by reference
#
list_sort () {
    test "$#" -ne 1 && return 1; eval "$1=\"\$(echo \"\$$1\" | sort -n)\""
}

##
# @brief    Sorts and reverses the sense of the list
# @usage    list_sort_reverse <lst>
# @out      Set the variable passed by reference
#
list_sort_reverse () {
    test "$#" -ne 1 && return 1; eval "$1=\"\$(echo \"\$$1\" | sort -nr)\""
}
