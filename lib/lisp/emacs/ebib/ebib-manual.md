Ebib is a program with which you can manage `biblatex` and BibTeX
database files without having to edit the raw `.bib` files. It runs in
GNU/Emacs, version 25.1 or higher.

It should be noted that Ebib is *not* a minor or major mode for editing
`.bib` files. It is a program in itself, which just happens to make use
of Emacs as a working environment, in the same way that for example Gnus
is.

# News

## Version 2.29, December 2020

  - Add support for [selectrum](https://github.com/raxod502/selectrum).

## Version 2.28, October 2020

  - Redesign keyword handling.
  - New timestamp format. This new format is more in line with ISO 8601
    and is sortable alphanumerically.

## Version 2.27, October 2020

  - Add customisation options `ebib-multiline-fields` and
    `ebib-fields-with-completion`.
  - Remove customisation option
    `ebib-edit-author/editor-without-completion` (use
    `ebib-fields-with-completion` instead).
  - Remove customisation options `ebib-file-field`, `ebib-url-field` and
    `ebib-doi-field`. There is no point in customising these fields,
    because `biblatex` expects these fields to contain their intended
    values.

## Version 2.26, September 2020

  - Copy key, entry, reference or citation to the kill ring (and system
    clipboard), for easy pasting to other buffers or applications.

## Version 2.25, June 2020

  - Allow adding the `.bib` file to Org mode links. The option
    `org-ebib-link-type` determines which kinds of Org links are created
    when doing `org-add-link` in an Ebib index buffer.

## Version 2.24, June 2020

  - Rename master/slave databases to main/dependent databases.

## Version 2.23, May 2020

  - Allow specifying .bib files with a file-local variable (esp. for
    non-LaTeX files).

## Version 2.22, February 2020

  - Improve prompting when inserting citations.
  - Allow selecting multiple entries when inserting a citation in a text
    buffer.
  - Automatically save a dependent database when an entry is added that
    is being inserted into a text buffer.

## Version 2.21, December 2019

  - New option `ebib-default-dir`: control the directory that Ebib uses
    as its default directory.

## Version 2.20, December 2019

  - Display the contents of an external note when the current entry has
    one. This behaviour can be customized with the option
    `ebib-notes-show-note-method`.
  - Rename `ebib-notes-use-single-file` to `ebib-notes-file`.

## Version 2.19, November 2019

  - Add command `ebib-jump-to-entry` (bound to `j` in the index buffer):
    quickly jump to any entry in any database using completion.
  - Allow a full URL in the `doi` field: when passing the DOI to a
    browser, `"https://dx.doi.org/"` is only added if the contents of
    the `doi` field does not start with `"http://"` `"https://"`.

## Version 2.17, June 2019

  - Create dependent databases, i.e., databases that share their data
    with a main database but which are saved as separate `.bib` files.
  - Use completion when editing certain fields, to make it easier to
    enter e.g., author or editor names consistently.

## Version 2.16, February 2019

  - Add command `ebib-download-url`: download pdf from a site in the
    `url` field.
  - Add command `ebib-import-file`: import local file into current
    database.
  - Update the menus.
  - Change display of the `file` field in the entry buffer and make file
    names clickable.
  - Bug fix: Do not look for alias fields in BibTeX databases. (Alias
    fields are only defined for `biblatex`).
  - Bug fix: `ebib-yank-field-contents` can now be repeated.

## Version 2.15, January 2019

  - Multiline field values can now be displayed in the entry buffer.
  - Do not warn about aliased entry types when loading `biblatex` files.
  - The abstract field is now treated as a multiline field by default,
    similar to the annote/annotation field.
  - Bug fix: `ebib-show-annotation` (bound to `A`) shows the contents of
    the annote field in BibTeX databases, not the annotation field
    (which is `biblatex`-specific).

## Version 2.14, December 2018

  - Change the user interface of the export functions. Calling one of
    the export functions now exports to another database. The user is
    asked to specify the database, enabling completion on the database
    names. To export to a file, the export functions have to be called
    with a prefix argument.
  - Bug fix: Handle overwriting the local BibTeX dialect correctly.
  - Bug fix: Add braces to the crossref field.

## Version 2.13, November 2018

  - Use a separate index buffer for each open `.bib` file. Filling the
    index buffer can be slow for large `.bib` files, because of the need
    to calculate the tabulated columns.

## Version 2.12.3, November 2018

  - Bug fix: when searching the database, only search visible entries.

## Version 2.12.2, November 2018

  - Bug fix: when editing the `crossref` field, offer keys from all
    databases as completion targets, not just the current one.

## Version 2.12, August 2018

  - Add an option to specify the default sort field and direction. It is
    no longer necessary to set the desired sort field as the first
    column in the index buffer, but Ebib still defaults to this if the
    default sort field and direction are not set explicitly.

## Version 2.11.12, July 2018

  - Use `biblatex`’s `Date` field, if present, for displaying the year.
  - Check for changed files on disk when saving all databases, not just
    when saving the current database.
  - Improve handling of multiple databases: `crossref` entries can now
    occur in other open databases as well, and searches with `/` can be
    continued in other databases.
  - Autogenerating keys is now on by default.

# Installation

## Package manager

The easiest way to install Ebib is to use Emacs’ package manager. Ebib
is available as a package from the [Melpa package
archive](http://melpa.org/). If you add the Melpa archive to your
`package-archives` list, you can install Ebib from the package manager.
This will also install the Info file so you can access the Ebib manual
within Emacs.

## Debian and Ubuntu

Users of Debian 9 or later and Ubuntu 16.10 or later can also use their
distro’s package manager: `apt-get install elpa-ebib`.

## Manual installation

It’s also possible to install Ebib manually. If you prefer this method,
then you probably know what you’re doing, so detailed instructions are
omitted here. Just be sure to also install the
[parsebib](https://github.com/joostkremers/parsebib) package, which Ebib
depends on.

## Starting Ebib

Once Ebib has been installed, you can start it with `M-x ebib`. This
command is also used to return to Ebib when you have put the program in
the background. You can bind this command to a key sequence by putting
something like the following in Emacs’ init file:

    (global-set-key "\C-ce" 'ebib)

You can of course choose any key combination you like. (In Emacs, key
combinations of `C-c` `<letter>` are reserved for the user, which means
that no package may set them.)

You can also call Ebib from an Eshell command line. This in itself is
entirely unspectacular (in Eshell, you can invoke any Emacs function),
but the nice thing is that you can then provide a filename to load. So,
provided a file `references.bib` exists in `~/Work/Papers/`, the
following command:

    ~/Work/Papers $ ebib references.bib

starts Ebib and loads the file `references.bib`.

# Getting Started

A BibTeX database is somewhat of a free-form database. A BibTeX entry
consists of a set of field-value pairs. Furthermore, each entry is known
by a unique key. The way that Ebib navigates this database is by having
two windows, one that contains a list of all the entries in the
database, and one that contains the fields and values of the currently
highlighted entry.

When Ebib is started (with `M-x ebib`), the current windows in Emacs are
hidden and the Emacs frame is divided into two windows. The top one
contains a buffer that is called the *index buffer*, while the lower
window shows the *entry buffer*. When a database is loaded, the index
buffer holds a list of all the keys in the database plus some additional
information for each entry: the author or editor, its year of
publication, and the title. You can move through the entries with the
cursor keys. In the entry buffer, the fields of the currently
highlighted entry are shown, with their values.

This manual first describes Ebib’s basic functionality, so that you can
get started with it. At times, reference will be made to later sections,
where more specific functions are described.

Ebib has a menu through which all of its functionality can be accessed.
Most functions are also bound to keys, but especially some of the lesser
used ones can (by default) only be accessed through the menu, though you
can always assign them to keys, if you prefer.

You can quit Ebib by typing `q`. You will be asked for confirmation, and
you will receive a warning if you happen to have an unsaved database.
You can also leave Ebib with the command `z`. However, unlike `q`, which
completely quits Ebib, `z` only lowers it, so that it remains active in
the background. The `.bib` files that you have opened remain loaded, and
you can return to them by typing `M-x ebib` again.

## Opening a `.bib` File

Loading a `.bib` file into Ebib is done with the command `o`. Ebib reads
the file that you specify and reports how many entries it found, how
many `@String` definitions it found, and whether a `@Preamble` was
found.

Every time Ebib reads a `.bib` file, it produces a few log messages.
These are written into a special buffer `*Ebib-log*`. If Ebib encounters
entry types in the `.bib` file that it doesn’t know, a warning will be
logged. If Ebib finds something that it cannot parse, it will log an
error. If warnings and/or errors occurred during loading, Ebib will
issue a message when it finishes loading the `.bib` and direct you to
the log buffer.

In order to parse `.bib` files, Ebib uses the entry type definitions of
`bibtex.el`, which is fairly complete, but if you use non-standard entry
types, you may need to customise `bibtex-biblatex-entry-alist` or
`bibtex-bibtex-entry-alist`, depending on which of the two you use. If
Ebib finds entry types in a `.bib` file that are not defined, those
entries will still be loaded, but their entry type is displayed using
Emacs’ `error` face. The most likely case in which this may happen is
when you load a BibTeX file without letting Ebib know the file is
`biblatex`-specific. By default, Ebib assumes that a `.bib` file it
loads is a BibTeX file. If you intend to use `biblatex` files, make sure
to read the section [`Biblatex` vs. Bibtex](#biblatex-vs.-bibtex).

When you open a `.bib` file, the directory in which you started Ebib is
the start directory for file name completion. If you always want Ebib to
assume a specific default directory, regardless of the directory in
which Ebib is actually started, you can customize the option “Default
Directory” (`ebib-default-directory`).

## Preloading `.bib` Files

Chances are that you will be doing most of your work with one or a few
`.bib` files, and you may find yourself opening the same file or files
every time you start Ebib. If so, you can tell Ebib to always load
specific `.bib` files on startup. To do this, specify the files in
Ebib’s customisation buffer, under the option “Preload Bib Files”
(`ebib-preload-bib-files`).

Files listed in `ebib-preload-bib-files` that do not have a full path
specification are searched for in the directories listed in the option
“Bib Search Dirs” (`ebib-bib-search-dirs`). By default, this option
only lists your home directory. Since this is most likely not where you
keep your `.bib` files, it makes sense to customize this option.

## Navigating a `.bib` File

Once you’ve opened a `.bib` file, all the entries in the file are shown
in alphabetical order (sorted by entry key, though this is customisable)
in the index buffer in the top Ebib window. The first entry is
highlighted, meaning it is the current entry. Its fields and their
values are shown in the entry buffer in the bottom Ebib window. The
first field is the type field, which tells you what kind of entry you’re
dealing with (i.e. `Book`, `Article`, etc.).

Below the type field, Ebib displays (up to) four sets of fields. The
first set are the so-called required fields, the fields that `biblatex`
requires to be filled. The second group are the optional fields, which
do not have to be filled but which `biblatex` will normally add to the
bibliography if they do have a value. The third group comprises the
so-called extra fields. These fields are usually ignored by `biblatex`
(note that `biblatex` and BibTeX normally ignore *all* fields they do
not know about), although there are bibliography styles that treat some
of these fields as optional rather than as extra. Extra fields are
defined in the user option “Extra Fields” (`ebib-extra-fields`). Lastly,
the fourth set of fields shown in the entry buffer are fields that exist
in the entry but are not defined as part of the entry type nor as extra
fields.

The first two groups of fields are different for each entry type, while
the third group is common to all entry types. You can use the extra
fields, for example, to add personal comments to the works in your
database. Ebib by default defines the following extra fields:
`crossref`, `url` (BibTeX only), `annote` (`annotation` for `biblatex`),
`abstract`, `keywords`, `file`, `timestamp`, and `doi` (BibTeX only).
`url` and `doi` are defined only for BibTeX, since `biblatex` defines
them as optional fields for most entry types. If these are not
sufficient for you, you can customise the option “Extra Fields”.

To move around in the index buffer, you can use the `up` and `down`
cursor keys, `p` and `n` and also the versions with the control key
`C-p` and `C-n`. Furthermore, `Space` and `PgDn` move a screenful of
entries down, while `b` and `PgUp` move in the other direction. Lastly,
`g` and `Home` move to the first entry, while `G` and `End` move to the
last one.

Ebib is not restricted to opening just one `.bib` file at a time. You
can open more files by just typing `o` again and entering the filename.
Ebib numbers the databases: the number of each database is shown in the
mode line of the index buffer, directly before the database name. The
keys 1–9 provide a quick way of jumping from one database to another.
Note that the numbering is dynamic: if you have three databases opened
and then close the second, database 3 becomes database 2.

With the `left` and `right` cursor keys, you can move to the previous or
next database. These keys wrap, so if you hit the `left` cursor key
while the first database is active, you move to the last database. If
you are done with a database and want to close it, type `c`. This closes
the current database, but it does not leave Ebib, and the other
databases you have open will remain so.

You can quickly jump to any entry in the database with the key `j`. This
ask you for an entry key (using completion) and then jumps to the
corresponding entry. This actually works across databases: the keys that
are offered for completion are the keys from all open databases. After
selecting a key, Ebib changes to the corresponding database and shows
the entry corresponding to the key.

If you use [selectrum](https://github.com/raxod502/selectrum),
[ivy](https://github.com/abo-abo/swiper) or
[helm](https://github.com/emacs-helm/helm) for completion, instead of
completing the entry key, you can type any part of the author/editor
names, of the title and the year of the entry you want to jump to. You
can also see the bibliography file to which the entry belongs. This is a
good way to search for a particular entry if you’re not sure of the
entry key. (In fact, with these completion systems, it is generally
unnecessary to remember the key of an entry, and you can customise the
option `ebib-index-columns` in order not to display it in the index
buffer.)

You can restrict the jump candidates to the current database by using a
prefix argument, i.e., by tying `C-u j`.

## Starting a New `.bib` File

If you want to start a new `.bib` file from scratch, you cannot just go
and enter entries. You first have to give the database a name. So, to
start a new database, type `o` first, and give the new file a name. Once
you have done this, you can start adding entries to the database.

# Editing the Database

Of course, being able to open and view `.bib` files is only half the
fun. One needs to be able to edit the files as well. Ebib’s essential
editing facilities are discussed here.

## Adding and Deleting Entries

To add an entry to a database, you type `a`. This creates a new entry
with a temporary key and puts you in the entry buffer, where you can
edit the fields of the entry. When you leave the entry buffer and return
to the index buffer (with `q`), the temporary key is replaced with a key
based on the contents of the `author` (or `editor`), `year` and `title`
fields. (Ebib uses the function `bibtex-generate-autokey` for this; see
that function’s documentation string for customisation options.)

If you prefer to specify a key yourself, you can unset the option
`ebib-autogenerate-keys`. With this option unset, Ebib will ask you for
a key when you create a new entry. Since the entry key must be unique,
Ebib will complain if you enter a key that already exists.

Note that if you should later decide that you want to change the key of
an entry, you can do so with the command `E`, and you can make Ebib
recreate an autogenerated key by pressing `!`.

Deleting an entry can be done in two ways. The key `d` deletes an entry
from the database. This command asks for confirmation, because once an
entry has been deleted in this way, it cannot be retrieved again.
Alternatively, you can use `k`, which kills the current entry, i.e., the
entry is deleted from the database and added to the kill ring.

The key `y` lets you yank an entry from the kill ring into the current
database. If the first element in the kill ring is not a properly
formatted BibTeX entry, the kill ring is simply rotated. This means that
you can press `y` again to (try and) add the next element in the kill
ring to the database.

In order for `y` to add a BibTeX entry to the database, the kill ring
item to be yanked must be a string that constitutes a properly formatted
BibTeX entry. Killing an entry from a database will result in such a
string (so you can easily move entries from one database to another by
killing and then yanking them), but killing a BibTeX entry from another
buffer or copying one from an outside source (e.g., a website) is also
possible. Furthermore, yanking also works with `@Preamble`, `@String`
and `@Comment` definitions.

## Editing Field Values

Editing the field values for an entry is done in the lower of the two
Ebib buffers, the so-called entry buffer. You can move focus to the
entry buffer and start editing field values by typing the command `e` in
the index buffer.

You can move between fields with the same keys that you use to move
between entries in the index buffer: the cursor keys `up` and `down`,
`p` and `n` or `C-p` and `C-n`. `Space` and `PgDn` move to the next set
of fields, while `PgUp` and `b` move to the previous set of fields. `g`
and `G`, and `Home` and `End` also work as expected. To finish editing
fields and move focus back to the index window, use `q`.

Editing a field value can be done with `e` or `RET`. For most fields,
Ebib simply asks you for a string value in the minibuffer. There is no
need to put braces `{}` around field values, Ebib adds them when it
saves the `.bib` file.

Fields for which it makes sense offer completion when you edit them. For
example, when you edit the `type` field, completion is offered on all
predefined entry types . Similarly, if you edit the `crossref` field,
Ebib offers completion on the keys in the databases currently open. Both
these fields require that you select one of the completion candidates.

The `keywords` field offers completion on all configured keywords (see
the section [Managing Keywords](#managing-keywords)) and the `file`
field offers file name completion (see [Viewing
Files](#viewing-and-importing-files)). Unlike the `type` and `crossref`
fields, however, they do not require that you select one of the
completion candidates.

For other fields that offer completion, the completion candidates are
the values of these fields in other entries in the databases that you’ve
opened. Offering these as completion candidates makes it easier to
ensure that you enter these values consistently. This of course mainly
makes sense for fields that have values that will occur more than once.
By default, apart from the fields already mentioned, completion is
offered for the `author`, `editor`, `journal`, `journaltitle`,
`organization` and `publisher` fields.

In the `author` and `editor` fields, completion takes into account that
these fields may contain more than one name. Each name is a separate
completion candidate, and when editing these fields, you can type the
individual names, Ebib will add the `"and"` that separates them.

If you want to edit a field value directly, without completion, you can
use a prefix argument: `C-u e` will let you edit a field as a plain
string. If you wish to disable completion permanently for particular
fields, or if you want to enable completion for other other fields, you
can customise the user option `ebib-fields-with-completion`. Note that
if this option contains the `author` field (which it does by default),
completion is also enabled for the `editor` field. Similarly, if it
contains the `crossref` field, completion is also enabled for the `xref`
and `related` fields.

## Editing Multiline Values

There are two other fields that Ebib handles in a special way when you
edit their value. These are the `annotation` field (or `annote` in
BibTeX), and the `abstract` field. Most field values normally consist of
a single line of text. However, because the `annotation` and `abstract`
fields are meant for creating annotated bibliographies, it would not be
very useful if you could only write one line of text in them. Therefore,
when you edit one of these fields, Ebib puts you in a so-called
*multiline edit buffer*. This is essentially a text mode buffer that
allows you to enter as much text as you like.

To store the text and leave the multiline edit buffer, type `C-c | q`.
If you want to leave the multiline edit buffer without saving the text
you have just typed, type `C-c | c`. This command cancels the edit and
leaves the multiline edit buffer. The text that is stored in the field
you were editing is not altered. (These keys are admittedly rather
awkward, but because of Emacs’ key binding conventions, it’s not
possible to set up something better by default. You can, of course,
change them yourself. See [Modifying Key
Bindings](#modifying-key-bindings) for details.)

Multiline values are not restricted to the `annotation` and `abstract`
fields. Any field (except the `type` and `crossref` fields) can in fact
hold a multiline value. To give a field a multiline value, use `m`
instead of `e`.

When a field has a multiline value, at most ten lines are shown in the
entry buffer. If the text is longer, an ellipsis indicator `[...]` is
added after the last line that is displayed. If you want to see the
whole contents of a multiline field, you can use `v`: this will display
the contents of the current field in a `*Help*` buffer (which can be
dismissed again with `q`). It’s possible to customise the way a
multiline value is displayed in the entry buffer. See the options
`ebib-multiline-display-function` and `ebib-multiline-display-max-lines`
for details.

For more details on working with multiline edit buffers, see [Multiline
Edit Buffers](#multiline-edit-buffers).

## Undefined Fields

`Biblatex` and BibTeX ignore fields that they do not know about, which
is a property that can be exploited to add any kind of information to an
entry. Ebib accommodates this by allowing fields with any name, not just
the ones that are predefined. Such undefined fields are displayed last
in the entry buffer, following the extra fields.

It is even possible to add such fields to an entry by pressing `a` in
the entry buffer. This asks for a field name and then a value. If you
make heavy use of this option, though, it may be better to define the
relevant fields through the user option `ebib-extra-fields`.

Note that if you delete the contents of an undefined field, the field
itself is also deleted. (In fact, the field remains in the database
until you close the database, but it will not be saved, so the next time
you load the `.bib` file, the field is gone.)

## Hidden Fields

`Biblatex` defines a large number of fields, many of which are optional
for most entry types. Displaying all these fields in the entry buffer
would not be very practical, because you are most likely interested in
only a few of them. For this reason, Ebib defines a (fairly large)
number of fields as ‘hidden’, meaning that they are not shown in the
entry buffer. You can make these fields visible with the key `H` in the
index buffer. Which fields are treated as hidden is controlled by the
option “Hidden Fields” (`ebib-hidden-fields`), which can be customised.

Most of the fields defined as hidden are `biblatex`-specific, because
BibTeX recognises a much smaller number of fields and there isn’t much
of a need to hide the lesser used ones. However, the functionality is
available: if you wish to use it, just add the relevant fields to the
option `ebib-hidden-fields`.

Note that a hidden field that has a value is always shown in the index
buffer. Hidden fields are only hidden in entries that don’t define a
value for them.

## Timestamps

Ebib provides the possibility to add a timestamp to every new entry,
recording the time it was added to the database. The timestamp is
recorded in the (extra) field `timestamp`, which is hidden by default.

You can tell Ebib to create timestamps by setting the option “Use
Timestamp” (`ebib-use-timestamp`) in Ebib’s customisation buffer. With
this option set, a timestamp is included in entries added to the
database with `a`. Ebib will also add a timestamp to entries imported
from a buffer or merged from a file, and to entries exported to another
database or to a file. When importing or exporting entries, existing
timestamps are overwritten. The logic behind this is that the timestamp
records the date and time when the entry was added to the database, not
when it was first created.

Note that if this option is unset, the timestamp of an entry is retained
when it’s imported or exported. Therefore, if you record timestamps and
want to im-/export entries without changing their timestamps,
temporarily unset this option, which can be done in the menu under
“Options”.

Ebib uses the function `format-time-string` to create the timestamp. The
format string that Ebib uses can be customised. The default string is
`"%Y-%m-%d %T (%Z)"`, which produces a timestamp of the form
`"2007-03-12 01:03:26 (CET)"`. This string is sortable and has the
additional advantage that it can be converted to Emacs’ internal time
representation with the function `date-to-time`. The format can be
customised; see the documentation for `format-time-string` on the
options that are available.

Adding timestamps in a format that `date-to-time` can parse makes it
possible to list the most recent additions to the database. Ebib
provides a function to do this: `ebib-list-recent`, which asks for a
number of days and lists the entries that were added since then. See
[Special Filters](#special-filters) for details.

## Copy, Cut (Kill), Paste (Yank), and Delete

A few more commands are available when you’re in the entry buffer
editing field values. The commands `c`, `k` and `y` implement copy, kill
and yank: `c` copies the contents of the current field to the kill ring,
`k` kills the contents of the current field to the kill ring, and `y`
yanks (pastes) the most recently killed text in the kill ring. You can
type `y` repeatedly to get the same effect you get in Emacs when you
type `M-y` after an initial `C-y`.

Lastly, there is the command `d`, which deletes the contents of the
current field without storing the text in the kill ring. (It asks for
confirmation, though, just to make sure.)

Note that `y` only works when the current field does not have a value
yet. This is to prevent you from accidentally overwriting a field value.
If you do want to yank text into a field that already has a value,
simply hit `d` first to delete the text.

# Saving a Database

When you have undertaken any kind of editing action on a database, it is
marked as modified, which is indicated in the mode line for the index
buffer. A modified database can be saved by typing `s`. This saves the
database to the file it was loaded from without asking for confirmation.
(It is similar to `C-x C-s` in Emacs.) If you’re saving a file for the
first time after loading it, Ebib creates a backup file. (Ebib honours
`backup-directory-alist` when saving backups. Note that you can also
disable backups altogether with the option `ebib-create-backups`.)

If you want to force-save a database that has not been modified, you can
use a prefix argument: `C-u s`. In either case, however, Ebib checks
whether the underlying file was modified and warns you if it was. (Ebib
does this by storing the `.bib` file’s modification time when reading
the file and comparing this time with the modification time when
requested to save the file.) If you also want to forego this check, use
a double prefix argument: `C-u C-u s`. This saves the file
unconditionally.

If you have multiple databases open, have made changes in more than one
of them, and want to save all of them without going through each
yourself, you can save all databases at once through the menu. You can
also save the database to another name, similar to `C-x C-w` in Emacs:
the new `.bib` file becomes associated with the database. The command
for this is `w`. This command can also be prefixed with `C-u` (or in
fact any other prefix argument) in order to overwrite any existing file
without asking for confirmation.

# `Biblatex` vs. BibTeX

BibTeX has long been a core part of the TeX ecosystem, but it has not
received any substantial update since 1988(\!) and it has next to no
support for languages other than English. Compared to BibTeX, `biblatex`
has an expanded set of entry types allowing for more diverse types of
references, a larger number of fields, and a much more sophisticated
system of field value inheritances. Most importantly, however,
`biblatex` (and its back-end `Biber`) has proper Unicode support.

For these reasons, the use of `biblatex` is highly recommended for
anyone using LaTeX. Still, for historical reasons, BibTeX is still the
default dialect, so if you intend to use `biblatex` files, you need to
configure Ebib.

## Setting the BibTeX Dialect

`Biblatex` files use the same `.bib` suffix that BibTeX files use.
Whether Ebib interprets a file as a BibTeX or a `biblatex` file is
determined by the user option “Bibtex Dialect” (`ebib-bibtex-dialect`).
Possible values for this option are `BibTeX` and `biblatex`, the default
being `BibTeX`. (These values are taken from the variable
`bibtex-dialect-list`.)

The dialect specified determines which entry types Ebib recognises and
which fields it expects. Reading a file with the wrong dialect setting
will most likely result in a series of “Illegal entry type” errors.
Note, however, that these entries will still be loaded and displayed,
but they will be highlighted with Emacs’ `error` face. Fields that are
not defined for the current dialect are displayed as undefined fields
(i.e., below all other fields in the entry buffer).

The option `bibtex-dialect` sets the default dialect, which is the
dialect that Ebib gives to newly created `.bib` files and which it
assumes for files that are not otherwise specified. If you wish to work
with a file that is in a different dialect than what you set as the
default, you can set the dialect for this particular file. To do this,
load the file and then set the dialect through the menu option «Ebib |
BibTeX Dialect» or with the command `M-x ebib-set-dialect`. You only
need to do this once for a file, because the setting is saved in the
`.bib` file in the local variable block. (If no local variable block
exists, one is created.) The setting is actually saved as a file-local
value for the variable `bibtex-dialect`, which means that if you should
open the file directly in `bibtex-mode`, Emacs will apply the dialect
setting as well.

The mode line of the index buffer shows the dialect that Ebib assumes
for the current database. Note that this does not necessarily mean that
the dialect is set in the `.bib` file: if the file does not have a
dialect setting, the mode line shows the default setting.

## Alias Types and Fields

The set of entry types defined by `biblatex` differs from the set used
by BibTeX. Mostly, `biblatex` adds new entry types, but there are a few
BibTeX entry types that have been dropped. For legacy reasons,
`biblatex` still recognises these entry types, but it treats them as
aliases for some of its own types. The relevant entry types are
`@conference` (treated as an alias for `@inproceedings`), `@electronic`
(alias for `@online`), `@mastersthesis` (alias for `@thesis` with the
`type` field set to ‘Master’s thesis’), `@phdthesis` (alias for
`@thesis` with the `type` field set to ‘PhD thesis’), `@techreport`
(alias for `@report` with the `type` field set to ‘technical report’)
and `@www` (alias for `@online`). If an entry has such an alias as entry
type, Ebib displays the entry type that `biblatex` treats it as in the
entry buffer. (For example, the entry type alias `phdthesis` is shown as
`phdthesis [==> Thesis]`.)

Similarly, a number of fields are deprecated but still accepted as
aliases. These are `address` (alias for `location`), `annote` (alias for
`annotation`), `archiveprefix` (for `eprinttype`), `journal` (for
`journaltitle`), `key` (for `sortkey`), `pdf` (for `file`),
`primaryclass` (for `eprintclass`), and `school` (for `institution`).
These aliases are also indicated in the entry buffer: for example, if an
entry has a `journal` field, its value is shown as the value of the
`journaltitle` field; a tag `[<== journal]` is placed after the field
value, indicating that the value is actually contained in the journal
field. The `journal` field itself is shown as an undefined field, i.e.,
after all other fields. Displaying the value twice this way means that
you can easily copy the value of the `journal` field to the
`journaltitle` field, if you wish to bring your entries into line with
`biblatex`’s conventions.

# The Entries List

By default, the index buffer displays the list of entries in the
database in a table format using the entry key, and the author, year and
title fields of each entry. The entries are sorted in ascending order on
the first column, which by default is the entry key. You can sort the
entries on one of the other columns using the keys `<` and `>`. The
former performs an ascending sort (smallest to largest, hence the
smaller-than sign), the latter a descending sort. They both ask you for
the column to sort on. Restoring the default sort can be done with `=`.

The fields that are displayed in the index buffer can be customised with
the user option `ebib-index-columns`. Each element in this option
describes a column and consists of the field to display (which is also
the column label), the width of the column and a flag indicating whether
the column can be sorted. You can add or remove fields, or reorder the
existing ones.

You can use any `biblatex` (or BibTeX) field to define a column in the
index buffer. There are a few column label that do not correspond
directly to a field name, however. For example, the column label `"Entry
Key"`, which displays the entry key, is not a `biblatex` field.
Similarly, there is a column label `"Author/Editor"`, which displays the
contents of the author field if it is not empty, and the contents of the
editor field otherwise. Furthermore, the column label `"Year"` does not
simply display the contents of the year field. Rather, it first checks
the contents of the date field, which is `biblatex`’s replacement of the
year field, and extracts the first year in it. Only if the date field is
empty does it display the year field.

Three other column labels have special behaviour: `"Title"`, `"Doi"`,
and `"Url"`. These do display information from the fields they
correspond with, but in a special way: `"Title"` tries to make the title
look nice by removing braces and LaTeX commands (including their
optional arguments) and by displaying the arguments of `\emph`,
`\textit`, `\textbf` and `\textsc` in italic, bold or caps. `"Doi"` and
`"Url"` don’t display the contents of these fields, but instead yield a
clickable string `"www"`. Clicking on `"www"` takes you to the relevant
web page.

The final predefined column label is `"Note"`. This does not, however,
display the contents of the note field. Rather, it checks whether the
entry in question has an external annotation (see [Notes
Files](#notes-files)). For those entries that have an annotation, the
`"Note"` column will display a (clickable) `"N"`. Keep in mind, though,
that if you keep your notes in a single file, adding this column to the
index display can slow down the creation of the index buffer (and thus
Ebib’s start-up). If you wish to use this column, it is probably best to
keep notes in separate files.

You can define new column labels and redefine the existing ones by
customising the option `ebib-field-transformation-functions`. Note that
`"Title"`, `"Doi"`, `"Url"`, and `"Note"` are actually defined through
this option. `"Entry Key"`, `"Author/Editor"`, and `"Year"` are not
(they are hard-coded), but they can be overridden by adding an entry for
them in `ebib-field-transformation-functions`.

The first column defined in `ebib-index-colums` is the column on which
the entries are sorted by default, i.e., when the database is first
opened and when you press `=`. You can change the default sort field and
the default sort direction (which is ascending, i.e., A-Z and 0-9) by
customising the option `ebib-index-default-sort`.

# Searching

Ebib provides several ways of searching through your database(s). To
search for a particular entry, you can use the command `j`
(`ebib-jump-to-entry`). If you are looking for a particular string
(regular expression), you can use `/` (`ebib-search`), which searches
through the database entry by entry. Finally, a more powerful search
method is offered by the filter mechanism, which allows you to filter
your database on arbitrary criteria.

## Simple Searches

If you want to look for a particular entry, the easiest way to do this
is to use `j`. This command (`ebib-jump-to-entry`) asks for an entry
key, offering completion while you type. Note that you can use this
command to search for an entry in all open databases. If you want to
restrict it to just the current database, use a prefix argument: `C-u
j`.

If you use [selectrum](https://github.com/raxod502/selectrum),
[ivy](https://github.com/abo-abo/swiper) or
[helm](https://github.com/emacs-helm/helm), this method is actually very
convenient, because the completion is more sophisticated: you can search
not on entry key but on any part of the author/editor name, the title
and the year.

If you want to search the entire contents of your entries, not just the
author/editor names and the titles, you can use `/`. This command
(`ebib-search`) searches for a string (more precisely, a regular
expression) starting from the current entry (i.e., *not* from the first
entry) and will display the entry with the first occurrence of the
search string that it finds. All the occurrences of the search string in
that entry are highlighted.

Ebib searches all the fields of each entry. It is not possible with `/`
to specify the fields to search. Note that if the search term is found
in a field with a multiline value, Ebib will highlight the ellipsis
symbol `[...]` that is displayed after the last line of the field value.
When the search term is found, Ebib gives a message saying so, similarly
if the search term was not found.

A search term may of course appear more than once in the database. To
search for the next occurrence, type `RET`. This continues searching for
the search term in the rest of the database. Again, the first entry
found to contain the search string is displayed. Note that the search
does not wrap: if the end of the database is reached, Ebib stops
searching and informs you that no further occurrence of the search
string was found. If you want to continue searching from the top, type
`g` and then continue the search with `RET`.

Note that once you’ve started a search with `/`, Ebib activates a
transient key map called `ebib-search-map`. It is this map that holds
the binding for `RET` to continue searching after the current entry and
of the key `g` to jump to the top of the database. There are also
bindings for the left and right cursor keys, which take you to the
previous and next database, so you can continue searching there.

Exiting a search (i.e., getting rid of the transient key map) is done by
pressing any key other than `RET`, `g` or the left/right cursor keys.
The search is ended and the command associated with this key is executed
normally. If you want to repeat a previous search, you can pass a prefix
argument to `/`. So typing `C-u /` starts searching for the previous
search string again.

Note that if you start a search in a filtered database (i.e., a database
in which not all entries are visible; see the next section), only the
visible entries are searched. If the search string is present in the
database but not in one of the visible entries, Ebib will respond with a
“search string not found” message.

## Filters

Ebib also has a much more sophisticated search mechanism that makes use
of *filters*. A filter is basically a search expression that selects
entries from the current database. When you apply a filter to a
database, only the entries that match are shown. With filters, you can,
for example, select all entries from a database that contain the string
“Jones” in their `author` field. A filter can be as complex as you
want: you can select all entries that do *not* contain “Jones” in the
`author` field, or all entries that contain “Jones” in either the
`author` or the `editor` field, or all entries that contain “Jones” in
the `author` field, and “symbiotic hibernation” in the `keyword` field,
etc. Basically, the filter can consist of an arbitary number of search
criteria combined with the logical operators `and`, `or` and `not`.

### Simple Selection

Creating a filter is simple: press `&`, and Ebib will ask you for a
field to select on, and for a regular expression to select with. So if
you want to select all entries that contain `"Jones"` in the `author`
field, you press `&` and type `author` as the field and `Jones` as the
regexp to filter on. Ebib then runs this filter on the database, and
only shows those entries that match the filter. To indicate that a
filter is active, the active filter is displayed in the mode line of
index buffer. (The filter can be displayed in Lisp form, if you prefer:
customise `ebib-filters-display-as-lisp` to do so.)

If you don’t want to filter on one specific field but rather want to
select all entries that match a certain regexp in any field, you can
type `any` as the field to filter on. So specifying `any` as the field
and `Jones` as the regexp will give you all entries that have a field
that contains `"Jones"` in them.

Note that you can also select items based on their entry type. In order
to do that, you need to specify `=type=` as the field to search, which
is the field name under which Ebib stores the entry type internally.
(There is also a “normal” field called `type`, hence the equal signs.)
If you search the `=type=` field, only exact matches are returned, so if
you search for `book`, only the entries that are of type `book` are
returned, not those of type `inbook`. You can use `TAB` completion in
this case, by the way.

If you specify the `keywords` field, the keywords associated with your
database are available for `TAB` completion as well. Though you can
enter any search term, of course.

### Complex Filters

Once you have filtered your database, you can refine or extend it. For
example, suppose you have a filter selecting all entries with `"Jones"`
in the `author` field and want to add all entries that have `"Jones"` in
the editor field to your selection. In this case you need to do a
logical `or` operation: you want to select an entry if it contains
`"Jones"` in the `author` field (which you already did) *or* if it
contains `"Jones"` in the `editor` field.

A short sidenote: the first impulse in a case like this might be to use
`and` instead of `or`: after all, you want to select all entries that
contain `"Jones"` in the `author` field *and* all entries that contain
`"Jones"` in the `editor` field. However, the filter that you build up
is used to test each entry *individually* whether it meets the selection
criterion. An entry meets the criterion if it contains `"Jones"` in the
`author` field *or* if it contains `"Jones"` in the `editor` field.
Therefore, `or` is the required operator in this case. If you would use
`and`, you would only get those entries that contain `"Jones"` in both
the `author` *and* `editor` fields (i.e., most likely none at all).

To perform a logical `or` operation, press the key `|`. As before, you
will be asked which field you want to filter on, and which regexp you
want to filter with. Ebib will then update the index buffer.

It is also possible to perform a logical `and` on the filter. Use this
if you want to select those entries that contain `"Jones"` in the
`author` field and e.g. `"symbiotic hibernation"` in the `keyword`
field. A logical `and` operation is done with the key `&`. (Note: this
is the same key that is used to create the filter. In fact, you can
create a filter with `|` as well: when used in an unfiltered database,
`&` and `|` are equivalent. They are only different when a filter is
already active.)

Both the `&` and `|` commands can be used with the negative prefix
argument `M--` (or `C-u -`, which is identical). In this case, the
search criterion is negated. That is, the negative prefix argument
performs a logical `not` operation on the search criterion. For example,
if you want to select all entries from a database that do *not* contain
“Jones” in the `author` field, you can do this by typing `M-- &` and
then filling out the relevant field and regexp.

There is another way of performing a logical `not` operation, which is
only available when a filter is active: by pressing the key `~`, you
invert the current filter. That is, if you have a filtered database with
all the entries containing `"Jones"` in the `author` or in the `editor`
field, and you press `~`, the selection is inverted, and now contains
all entries that do *not* have `"Jones"` in the `author` or `editor`
field.

Although `~` and the negative prefix argument to `&` or `|` both perform
logical `not` operations, they are *not* equivalent: `~` negates the
entire filter built up so far, while the negative prefix argument only
negates the single selection criterion you enter with it.

When a filter is active, the filter itself is displayed at the top of
the index buffer. If the index window is too small to display the entire
filter (which can easily happen if Ebib is set to split the frame
vertically rather than horizontally), you can press `F v` (uppercase
`F`, small `v`), which will display the filter in the minibuffer.

To cancel the filter and return to the normal view of the database,
press `F c`. For convenience, this action is also available with `c`,
which normally closes a database. If a filter is active, however, it
simply cancels the filter. (If you find this behaviour confusing, you
can rebind the `c` key to the function `ebib-close-database`. See
[Modifying Key Bindings](#modifying-key-bindings) for details.)

### Storing and Saving Filters

When you cancel a filter, it is automatically stored so that it can be
reapplied later. To reapply a filter, type `F L`. This will reapply the
last used filter regardless of which database you’re in. That is, you
can use this to search more than one database without having to type the
filter over and over.

However, Ebib only stores one filter this way. If you want to store more
filters, you have to name them. You can store the currently active
filter or the last used filter with `F s`. Ebib will ask you for a name
for the filter in order to identify it later. (By default, filter names
are case-insensitive, but if you prefer to use case-sensitive filter
names, you can unset the option `ebib-filters-ignore-case`.) When Ebib
is closed, all stored filters are saved to a file and they’re
automatically reloaded when you open Ebib again. Stored filters are not
associated with a particular database: once a filter is stored, it is
available to all databases.

You can apply a stored filter with `F a`. This will ask for the name of
a filter and apply it to the current database. You can extend the filter
in the normal way, though the changes will not be stored automatically.
To store it, type `F s` again. You can store the extended filter under
the old name, in which case Ebib will ask you for confirmation, or under
a new name, which will store it as a new filter, keeping the old one.

The file that Ebib uses to store filters is `~/.emacs.d/ebib-filters`,
although that can of course be customised (`ebib-filters-default-file`).
As mentioned, stored filters are saved automatically when Ebib closes,
but you can also save them manually with `F S`. Note that if there are
no stored filters when Ebib is closed (or when you press `F S`), the
file is deleted.

You can also save your filters to a different file with `F w`. Such a
filter file can be reloaded later with `F l`. If you load filters from a
file while you still have stored filters, you are asked if you want to
replace them completely or if you want to add the new filters to the
existing ones. In the latter case, however, filters whose name conflict
with existing filters are not loaded. (Ebib will log a message about
this when it happens.)

To see what filters are currently stored, use `F V`. If you want to
rename a filter, you can do so with `F R`.

Note that cancelling a filter with `F c` does not delete it from the
list of stored filters, it will remain available for later application.
If you want to delete a filter from the list of stored filters, use `F
d`. You can also delete all stored filters with `F D`. These deletion
commands do not ask for confirmation, but if you delete any filters by
accident, you can reload them from `~/.emacs.d/ebib-filters` with `F l`.

### Special Filters

Filters are essentially Lisp expressions that consist of the functions
`and`, `or`, and `not`, together with a special macro `contains`.
However, filters are not limited to these forms. They can essentially
contain any Lisp expression. It is not possible to create such special
filters interactively, but it is possible to write such filters and put
them in a filter file, or to write a function that creates such a
special filter.

A filter is a Lisp expression that should return either `t` or `nil`,
indicating whether the entry being tested matches the filter or not. The
contents of the entry is available in a variable `ebib-entry`. This
variable is given a value by the function that runs the filter, but it
is not passed as an argument. Rather, it is a dynamic variable, which
means that the file that defines the filter function should declare the
variable with `(defvar ebib-entry)`. When the filter is run, the value
of `ebib-entry` is an alist of fields and their values. These include
the fields `=key=` and `=type=` for the entry key and type. For example:

    (("author" . "{Noam Chomsky}")
     ("title" . "{Syntactic Structures}")
     ("publisher" . "{The Hague: Mouton}")
     ("year" . "{1957}")
     ("timestamp" . "{2007-12-30}")
     ("file" . "{c/Chomsky1957.pdf}")
     ("=type=" . "book")
     ("=key=" . "Chomsky1957"))

### An Example: Listing Recent Additions

One special filter is included with Ebib. It filters recent additions to
the database. The command that creates the filter is `ebib-list-recent`:

``` lisp
(defun ebib-list-recent (days)
  "List entries created in the last DAYS days."
  (interactive "nNumber of days: ")
  ;; Save the database's current filter, if there is one.
  (let ((filter (ebib-db-get-filter ebib--cur-db)))
    (when filter (setq ebib--filters-last-filter filter)))
  (let*
      ;; Calculate the from-date in Emacs' time format.
      ((date (time-subtract (current-time) (days-to-time days)))
       ;; Create a Lisp expression that will function as the filter.
       (filter `(ebib--newer-than (quote ,date))))
    ;; Install it as the current database's filter.
    (ebib-db-set-filter filter ebib--cur-db)
    ;; Update the current entry key.
    (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
    ;; Update the display, so that only filtered entries are visible.
    (ebib--update-buffers)))
```

First, this function saves the current filter if there is one. It then
calculates a date in Emacs’ internal time format by subtracting the
number of days provided by the user from the current date and creates a
Lisp expression that tests whether an entry’s timestamp is earlier or
later than this date. This expression is then installed as the filter
for the current database. A call to `ebib--update-buffers` then updates
the display, taking the filter into account.

The function `ebib--newer-than` is defined as follows:

``` lisp
(defun ebib--newer-than (date)
  "Function for use in filters.
Return t if the entry being tested is newer than DATE.  DATE must
be a list of the format returned by `current-time' and is
compared to the timestamp of the entry being tested.  If the
entry has no timestamp, or a timestamp that cannot be converted
into a date representation, return nil."
  (let ((timestamp (cdr (assoc-string "timestamp" ebib-entry))))
    (when (and timestamp
               (setq timestamp (ignore-errors (date-to-time timestamp))))
      (time-less-p date timestamp))))
```

This function obtains the time stamp of the entry being tested from the
variable `ebib-entry` and then tries to convert it to Emacs’ time
format. If successful, it compares this time to the date passed as an
argument and returns `t` if the latter precedes the former.

### Properties of Filtered Databases

When a filter is active, there are a few things that are not possible or
function differently. First, it is not possible to add or delete
entries, either interactively or by merging or exporting. Exporting from
a filtered database or saving a filtered database is also disabled.
Editing existing entries is possible, however. Note that if the entry
doesn’t match the filter anymore after the edit, it doesn’t disappear
from view. For that, you need to reapply the filter with `F r`.

It is also possible to mark entries. Marked entries stay marked when you
cancel the filter, so in order to do something with all the entries
matching a filter, you can mark them all in the filter view with `C-u
m`, then cancel the filter and perform an action on them.

If a database has an active filter, the save command is disabled,
because it would not be clear whether you want to save the entire
database or just the filtered entries. If you want to save only the
filtered entries to a file, you can use the command `w` (or the menu
option “Database | Save As”). This also saves the `@String`, `@Preamble`
and `@comments`, as well as any file-local variables, so you will have a
self-contained `.bib` file with only the filtered entries. In order to
save the entire database, you need to cancel the filter. (After saving,
you can reapply the filter with `F L`, of course.)

One final note: of all the filter-related commands, `~`, `F c`, `F r`,
`F s` and `F v` are only available when a filter is active. The other
commands operate on the stored filters and can be used when no filter is
active.

# Inserting Citations into a Text Buffer

When you’re in a text buffer and you have Ebib open in the background
(i.e., you lowered Ebib with `z`), you can insert a citation with the
command `ebib-insert-citation`. This command asks for a key and inserts
a citation with that key in a (user-selectable) form that is appropriate
for the current buffer. By default, this is set up for LaTeX and
[Pandoc](http://johnmacfarlane.net/pandoc/) Markdown buffers. There is
some support for [Org mode](http://orgmode.org) as well, as discussed
below.

When you invoke `ebib-insert-citation`, Emacs prompts you for a key from
the database(s) associated with the current buffer and for a citation
command to use. You can use `TAB` completion when typing the key. If you
have [selectrum](https://github.com/raxod502/selectrum),
[ivy](https://github.com/abo-abo/swiper) or
[helm](https://github.com/emacs-helm/helm) installed, however, Ebib uses
a more sophisticated method: instead of typing just the key, you can
type (parts of) the author name, publication year and title in order to
find the reference you wish to cite.

You can define different citation commands for each type of file that
you use. That is, you can have one set of citation commands for LaTeX
files, another set for Org files, etc. For LaTeX buffers, the citation
commands that have been predefined are those used by `biblatex` (well,
the most common ones, anyway). If you use BibTeX, you may need to
customise the option `ebib-citation-commands`, as discussed below,
[Defining Citation Commands](#defining-citation-commands).

For Markdown buffers, three commands have been predefined: `text`, which
inserts a citation of the form `@Jones1992`, `paren`, which inserts a
citation of the form `[@Jones1992]` and `year`, which inserts
`[-@Jones1992]`. Since these are the only types of citations that Pandoc
Markdown knows, you shouldn’t need to change anything.

Ebib also provides a way to insert citations into a buffer from within
Ebib. If you’re in the index buffer and press `i`, Ebib asks you for a
buffer to insert the citation into (which defaults to the buffer you
started Ebib from, or the buffer you previously inserted an entry into),
a citation command and also any optional arguments, and then inserts a
citation at the current cursor position in the buffer you’ve supplied.

## Citations with multiple keys

Most citation commands in LaTeX can take multiple keys. To add more than
one key to a citation, you can mark them in Ebib’s index buffer with `m`
and then insert them into a text buffer with `i`. If you use ivy or
helm, the standard method that these packages provide for selecting and
acting on multiple candidates can be used if you insert a citation from
within your text buffer with `ebib-insert-citation`. If you use
selectrum or Emacs’ built-in completion method, you can enable selection
of multiple keys by setting the option `ebib-citations-insert-multiple`.
With this option set, you can select multiple keys when calling
`ebib-insert-citation` in the following way: start typing a key,
complete it with `TAB`, type `SPC` and start typing the next key, again
with the ability to complete it with `TAB`. When you have entered all
the keys you wish to select, press `RET`.

This method of multiple selection is somewhat cumbersome and it does not
use Emacs’ standard completion function `completing-read`, so it is
likely unfamiliar to most users. For these reasons, it is not enabled by
default, but it is there if you want to use it.

## Key Bindings

Of course, the easiest way to use the commands discussed here is to bind
them to a key sequence. For example, the following binds `C-c b` to
`ebib-insert-citation` in AUCTeX’s LaTeX mode:

    (define-key 'LaTeX-mode-map "\C-cb" 'ebib-insert-citation)

Note that commands of the form `C-c <letter>` are reserved for the user,
and should therefore not be set by any package. For this reasons, Ebib
does not set this command itself.

`ebib-insert-citation` recognises the major mode of the buffer it is
called from and uses this information to determine which kinds of
citations to insert. So you can bind the `ebib-insert-citation` to the
same key sequence in every text mode in which you use citations and Ebib
will do the right thing.

## Defining Citation Commands

Citation commands are defined for specific major modes. Ebib defines
commands for `latex-mode` (a.k.a. `LaTeX-mode`), for `org-mode` and for
`markdown-mode`. As mentioned, the commands defined for LaTeX are those
used by `biblatex`. If you use something else, you may need to set up
some commands yourself. This can be done by customising the option
“Citation Commands” (`ebib-citation-commands`).

Each command consists of an identifier, which you type when Ebib prompts
you for a citation command, plus a format string, which is used to
create the actual citation command.

The identifier should be a simple string which you can type easily when
Ebib asks you for a citation command (`TAB` completion is available,
though). The format string can contain a few directives, which are used
to add the citation key and any optional arguments. The following
directives are recognised:

  - `%K`  
    the entry key to be inserted.
  - `%A`  
    an argument, for which the user is prompted.
  - `%<...%>`  
    optional material surrounding a `%A` directive.
  - `%(...%<sep>)`  
    a so-called *repeater*, which contains material that can be
    repeated. If present, the repeater must contain the entry key
    directive `%K`.
  - `%D`  
    a description, for which the user is prompted. Mainly for use in Org
    citations.

In the simplest case, the format string contains just a `%K` directive:
{% raw %} `\cite{%K}`. {% endraw %} In this case, `%K` is replaced with
the citation key and the result inserted. Usually, however, citation
commands allow for optional arguments that are formatted as pre- or
postnotes to the citation. For example, using the `biblatex` package,
you have citation commands available of the form:

    \textcite[cf.][p. 50]{Jones1992}

In order to be able to insert such citations, the format string must
contain `%A` directives:

{% raw %}

    \textcite[%A][%A]{%K}

{% endraw %}

With such a format string, Ebib asks the user to provide text for the
two arguments and inserts it at the locations specified by the
directives. Of course, it is possible to leave the arguments empty (by
just hitting `RET`). With the format string above, this would yield the
following citation in the LaTeX buffer:

    \textcite[][]{Jones1992}

The empty brackets are completely harmless, because LaTeX will simply
ignore the empty arguments. However, you may prefer for the brackets not
to appear if the arguments are empty. In that case, you can wrap the
brackets and the `%A` directives in a `%<...%>` pair:

{% raw %}

    \textcite%<[%A]%>%<[%A]%>{%K}

{% endraw %}

Now, if you leave the arguments empty, Ebib produces the following
citation:

    \textcite{Jones1992}

Note however, that this format string is problematic. If you fill out
the first argument but not the second, Ebib produces the wrong format
string:

    \textcite[cf.]{Jones1992}

If only one optional argument is provided, `biblatex` assumes that it is
a postnote, while what you intended is actually a prenote. Therefore, it
is best not to make the second argument optional:

{% raw %}

    \textcite%<[%A]%>[%A]{%K}

{% endraw %}

This way, the second pair of brackets is always inserted, regardless of
whether you provide a second argument or not.

`Biblatex` commands also accept multiple citation keys. When you call
`ebib-insert-citation` from within a LaTeX buffer, you can only provide
one key, but when you’re in Ebib, you can mark multiple entry keys and
then use `i` to insert them to a buffer. In this case, Ebib asks you for
a separator and then inserts all keys into the position of `%K`:

    \textcite{Jones1992,Haddock2004}

It is, however, also possible to specify in the format string that a
certain sequence can be repeated and how the different elements should
be separated. This is done by wrapping that portion of the format string
that can be repeated in a `%(...%)` pair. Normally, you’ll want to
provide a separator, which is done by placing it between the `%` and the
closing parenthesis:

{% raw %}

    \textcite[%A][%A]{%(%K%,)}

{% endraw %}

This format string says that the directive `%K` can be repeated and that
multiple keys must be separated with a comma. The advantage of this is
that you are no longer asked to provide a separator.

It is also possible to put `%A` directives in the repeating part. This
is useful for `biblatex`’s so-called *multicite* commands that take the
following form:

    \footcites[cf.][p. 50]{Jones1992}[][p. 201]{Haddock2004}

Multicite commands can take more than one citation key in braces `{}`
and each of those citation keys can take two optional arguments in
brackets `[]`. In order to get such citations, you can provide the
following format string:

{% raw %}

    \footcites%(%<[%A]%>[%A]{%K}%)

{% endraw %}

Here, the entire sequence of two optional arguments and the obligatory
citation key is wrapped in `%(...%)`, so that Ebib knows it can be
repeated. If you now mark multiple entries in Ebib, press `i` and select
the `footcites` command, Ebib will put all the keys in the citation,
asking you for two arguments for each citation key.

Of course it is also possible to combine parts that are repeated with
parts that are not repeated. In fact, that already happens in the
previous example, because the part `\footcites` is not repeated. But the
part that is not repeated may contain `%A` directives as well:

{% raw %}

    \footcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)

{% endraw %}

Multicite commands in `biblatex` take two additional arguments
surrounded with parentheses. These are pre- and postnotes for the entire
sequence of citations. They can be accommodated as shown.

Lastly, a citation command can also contain a `%D` directive. This is
mainly for use in Org citations, which take the form
`[[ebib:<key>][<description>]]`. The description is not an argument to
the citation command but the string that will be displayed in the Org
buffer.

## Associating a Database with a Text Buffer

The commands `ebib-insert-citation` and `ebib-entry-summary` must
consult the database or databases loaded in Ebib, and Ebib tries to be
smart about which database(s) to consult. How Ebib decides which
databases to consult depends on the major mode of the text buffer.

In a LaTeX buffer, Ebib looks for `\addbibresource` commands or a
`\bibliography` command and uses the files specified in them. If the
variable `TeX-master` is set (which is used by AUCTeX to keep track of a
file’s master file), the master file is searched instead.

In non-LaTeX buffers, Ebib first checks if `pandoc-mode` is active; if
it is, Ebib uses the value of the `bibliography` option. If
`pandoc-mode` is not used, Ebib simply uses all databases that are
currently open.

Keep in mind that Ebib tries to determine the relevant databases only
once per buffer. It stores the result of this search and uses it the
next time either of these commands is used. Therefore, if you add,
rename or remove bibliography files in your project, you may need to
reload the file (use `M-x revert-buffer` or `C-x C-v RET`).

You can override Ebib’s automatic association of `.bib` files to a
buffer by setting the variable `ebib-local-bibfiles` to a list of files.
This can be done as a file-local or a directory-local variable, or as a
customisable option.

## Links and Citations in Org buffers

Currently, Org mode does not have real support for citations (though
support is planned for a future release). Ebib provides a way to add
links to BibTeX entries to an Org file which, with some coaxing, can be
used as citations.

If you call `ebib-insert-citation` in an Org buffer, you can add a link
to an entry in a `.bib` file that’s open in Ebib. The link has the form
`[[ebib:<key>][<description>]]`. The description is user-provided
string, which you are prompted for, but a default description is
provided, which you can accept by pressing `RET`. This default
description is created by the function in
`ebib-citation-description-function` which uses the author name and
publication year to create a description.

If you use this type of Org link, you may want to load the `org-ebib`
package, which allows you to open Ebib with `org-open-at-point` (by
default bound to `C-c C-o`), taking you to the entry in the link
(provided its database is opened in Ebib).

The `org-ebib` package also allows you to create Org links to Ebib
entries with `org-store-link` when you’re in the entry buffer. Links
created in this way have the same form, but they can also specify the
`.bib` file containing the entry by adding an `@` sign after the key and
the name or full path of the file. Which type of link is produced is
controlled by the user option `org-ebib-link-type`.

# Copying Entries to the Kill Ring and System Clipboard

Ebib offers several ways to copy an entry to the kill ring (and the
system clipboard), which you can then insert into another buffer or
another application. You can copy the entry key (`C k`), the entire
BibTeX entry (`C e`), a full reference as would appear in a list of
references (`C r`) or a citation, by default of the Author-Year type (`C
c`).

The functions that copy a reference or citation make use of templates
that specify how such a reference/citation should be formatted. These
templates can be customised: the relevant options are
`ebib-reference-templates` and `ebib-citation-template`. (The latter
should not be confused with `ebib-citation-commands`, which defines
templates for inserting citation commands into a LaTeX / Markdown / etc.
buffer.)

These templates are strings that contain directives for inserting
specific fields from the entry being copied. As an example, a simple
template for an author-year citation would be the following:

“{Author} ({Year})”

The directives are marked by braces {} around a field name. In the
resulting citation, they are replaced by the contents of the fields.
(The field names are case-insensitive, they could also be written as
`"{author} ({year})."`)

Alternative fields can be separated by a pipe bar `|`:

    "{Author|Editor} ({Date|Year})"

This template uses the `Author` field unless it’s empty, in which case
the `Editor` field is used. Similarly for the year: first the contents
of the `Date` field is checked. The `Year` field is used if the `Date`
field is empty.

If none of the fields in a directive has any contents, the directive is
discarded completely. Most reference templates for example include a
directive for the `Doi` or `Url` field:

    "{Author|Editor} ({Date|Year}). {\"Title\"}. {Publisher}. {Doi|Url.}"

If the `Doi` and `Url` fields are both empty, the directive is simply
ignored.

A directive may contain punctuation before or after the field name (or
sequence of field names), which is dropped if the field is empty. The
`{Doi|Url.}` directive in the previous example contains a full stop,
which is only included in the reference if the `Doi` or `Url` field is
present.

The contents of the fields is used literally, with two exceptions: the
`Date` field may contain a full date+time specification or even a date
range, but only the year (or the year of the first date in a date range)
is used. Similarly, the `Title` field is stripped of LaTeX markup.

# Main and Dependent Databases

If you want to create a `.bib` file from a larger database that only
contains the references of a particular paper, you can use a dependent
database. A dependent database, as the name suggests, depends on
another, real database that is called its main database. The dependent
database can only contain entries that also exist in its main database
and all the data of the entries is shared by both databases. If you edit
an entry in the dependent database, the edit shows up in the main one as
well, and vice versa.

Furthermore, if you have a text buffer that’s associated with a
dependent database, inserting an entry with `M-x ebib-insert-citation`
offers all entries of the main database for completion, not just the
ones that are already in the dependent database. If you select an entry
that is not in the dependent database yet, it is added to it. This way,
creating a separate database for a paper is easy: just create a
dependent database and associate it with the relevant text buffer. Then
insert citation commands as usual.

You can create a dependent database with the key sequence `M c` in the
database that you want to be its main database. Ebib asks you for a file
name and then creates a new empty database.

Adding new entries to a dependent database can be done as described, by
inserting citations in a text buffer. It’s also possible to add entries
in the usual way, i.e., by pressing `a` in Ebib’s index buffer. If you
do this in a dependent database, instead of creating a new entry, you
are prompted for an entry from the main database to add to the dependent
one.

It’s also possible to add entries to a dependent database from its main
database with the command `M a`. This command also works on marked
entries, making it possible to add multiple entries to a dependent
database in one go.

Deleting an entry in a dependent database only removes it from the
dependent database, not from the main database. If you delete an entry
from the main database that is also present in a dependent database, it
is removed from both, given that a dependent database can only have
entries that also exist in the main database.

A database can serve as the main database for more than one dependent
databases, but the reverse is not possible: each dependent database can
only have one main database.

If you save a dependent database, it is saved as a normal, standalone
`.bib` file that can be used with `biblatex` or BibTeX. When you reopen
the file in Ebib, a special comment at the top of the file makes sure
that Ebib recognises it as a dependent database and loads the main
database as well, if necessary. Note that when Ebib opens a dependent
database, it only reads the entry keys from the `.bib` file. The data of
each entry is taken from the main database. This means that if you edit
a dependent database’s `.bib` file outside of Ebib, the changes you make
are ignored when you open the file in Ebib.

# Cross-referencing

`Biblatex` has a sophisticated cross-referencing facility that Ebib
supports. Suppose you have an entry `Jones1998`, which appeared in a
book that is also in your database, say under `Miller1998`. You can tell
`biblatex` that `Jones1998` is contained in `Miller1998` by putting
`Miller1998` in the `crossref` field. When `biblatex` finds such a
cross-reference, all the fields of `Jones1998` that don’t have a value
inherit their values from `Miller1998`. At the very least, this saves
you some typing, but more importantly, if two or more entries
cross-reference the same entry, `biblatex` automatically includes the
cross-referenced entry in the bibliography (and puts a reduced reference
in the cross-referencing entries).

When you fill in the `crossref` field in Ebib, Ebib displays the values
of the cross-referenced entry in the entry buffer. To indicate that they
are just inherited values, they are marked with `ebib-crossref-face`,
which by default inherits from `font-lock-comment-face`. (You can
customise it, of course.) These values are merely displayed for
convenience: they cannot be edited. (They can be copied, however).

`Biblatex`’s inheritance rules depend on the field and on the types of
the cross-referencing *and* the cross-referenced entry. Thus it is
possible to specify that the `InBook` entry type can inherit a
`maintitle` field from the `title` field if the cross-referenced entry
is of type `MVBook`, and a `booktitle` field if the cross-referenced
entry is of type `Book`. The inheritance scheme for `biblatex` is
defined by the option `ebib-biblatex-inheritances`, which is set up with
the default inheritance relations defined by `biblatex`, but which can
be customised if needed.

BibTeX’s inheritance mechanism is much more simplistic. A field without
a value in a cross-referencing entry simply inherits the value of the
same-name field in the cross-referenced entry. There is no way to
specify that the `title` field should inherit from e.g., the `booktitle`
field. Therefore, Ebib does not provide a way to customise inheritance
in BibTeX files.

If you’re viewing an entry that has a cross-reference and you want to go
to the cross-referenced entry you can type `C`. This command reads the
value of the `crossref` field and then displays that entry. If you want
to do the reverse, i.e., see if the current entry is cross-referenced by
any other entries, you can use the same key `C`: if you type `C` on an
entry that does not have a cross-reference, Ebib makes the key of the
current entry the current search string and searches for its first
occurrence after the current entry. Note that after Ebib has jumped to
the first cross-referencing entry, you cannot type `C` again to find the
next one. (Instead, it would take you back to the cross-referenced
entry.) In order to find the next cross-referencing entry, you have to
type `RET`, as with a normal search. (Also, if the cross-referenced
entry appears alphabetically before the cross-referencing entry, you
need to type `g` and then `/`.)

Note that if you want to use `biblatex`’s (or BibTeX’s)
cross-referencing options, the option `ebib-save-xrefs-first` needs to
be set (which it is by default). This tells Ebib to save all entries
with a `crossref` field first in the `.bib` file. Without this,
cross-referencing will not work reliably.

# Marking Entries

Commands in the index buffer generally operate on one single entry. For
some commands, however, it may sometimes be useful to perform them on
more than one entry. This can be achieved by selecting entries. You can
select the entries you want to perform a command on with the key `m`.
This selects (or unselects) the current entry. Selected entries are
highlighted (using the face `ebib-selected-face`).

Commands for which it makes sense automatically operate on all marked
entries if there are any. Of the commands discussed so far, these are
`d` to delete entries and `i` to insert entries to a LaTeX buffer. (Note
that Ebib creates a single citation command with commas separating the
entry keys.)

With a prefix argument, i.e, with `C-u m`, you can unmark all entries
or, if there are no marked entries, mark all entries in the current
database.

# Printing the Database

Sometimes it may be useful to have a `.pdf` file or print-out of your
database. Although Ebib does not actually do the printing itself, it can
create a LaTeX file for you that you can compile and print. In fact,
there are two ways of doing this.

The print options are available in the Ebib menu when the index buffer
is active. You can print the entries as index cards or as a
bibliography.

If you print your entries as a bibliography, Ebib creates a simple LaTeX
document that essentially contains a `\nocite{*}` command followed by a
`\printbibliography` command, adding a `\addbibresource` command
referring to the current database. You can then run the usual sequence
of LaTeX, Biber, LaTeX, LaTeX on this file, creating a document
containing a list of all the references in your database. (Obviously,
BibTeX is also supported.)

If you choose to print as index cards, Ebib also creates a LaTeX file.
However, instead of simply providing a `\nocite{*}` command, this file
contains a `tabular` environment for each entry in the database listing
all the fields of that entry and their values.

The entries are separated by a `\bigskip`, but if you set the option
`Print Newpage` in the customisation buffer (or in the Print menu), the
entries are separated by a `\newpage`, so that every entry is on a
separate page. The latter option is useful when printing actual index
cards (though you’d probably have to change the page size with the
`geometry` package as well).

By default, the index cards only show single-line field values. That is,
multiline values are normally excluded. If you want to include multiline
values in the print-out, you have to set the option `Print Multiline` in
the Options menu or in Ebib’s customisation buffer. With this option
set, Ebib includes all multiline values in the LaTeX file that it
creates. Note however that Ebib does not change anything about the
formatting of the text in a multiline value. So if you plan to make
(heavy) use of this option, make sure that the way you type your text
conforms to LaTeX’s conventions (e.g. empty lines to mark paragraphs,
etc.) and doesn’t contain any characters such as `&` that are illegal in
LaTeX. (Or, alternatively, use LaTeX code in your multiline fields.)

As mentioned, when you “print” the database, Ebib really just creates a
LaTeX file. More precisely, it creates a temporary buffer and writes the
LaTeX code into it, and then saves the contents of that buffer to a
file. After it has done that, Ebib lowers itself and instruct Emacs to
open the file in a buffer, which will then be properly set up as a LaTeX
buffer. From there you can run LaTeX and view the result.

Before doing all this, Ebib asks you which file to write to. Be careful
with this: since this is supposed to be a temporary file, Ebib simply
assumes that if you provide a filename of an existing file, it can
overwrite that file without warning\!

A better way to tell Ebib which file to use is to set the option “Print
Tempfile” in Ebib’s customisation buffer to some temporary file. When
this option is set, Ebib will always use this file to write to, and will
not ask you for a filename anymore.

Note that both print options operate on all entries of the database or
on the selected entries.

The option “Print Preamble” and “LaTeX Preamble” allow you to customize
the preamble of the LaTeX file that is created.

# Calling a Browser for URLs and DOIs

With most scientific literature nowadays being available on-line, it is
common to store URLs and DOIs in a BibTeX database. `Biblatex` has
standardised fields for this information, for BibTeX, Ebib adds these
fields to each entry.

To open a URL in your default browser, you can type `u` in the index or
entry buffer. Ebib takes the URL stored in the `url` field of the
current entry and passes it to your browser. If you happen to have more
than one URL stored in the relevant field, Ebib will ask you which one
you want to open. Alternatively, you can use a prefix argument: typing
`M-2 u` sends the second URL to your browser.

It is not even necessary that the relevant field contains *only* URLs.
It may contain other text mixed with the URLs: Ebib simply searches the
URLs in the field and ignores the rest of the text. Ebib considers every
string of characters that starts with `http://` or `https://` and that
does not contain whitespace or any of the characters `" ' ; <` or `>` as
a URL. The semicolon is included here even though it is actually a valid
character in URLs. This is done for consistency, because the semicolon
(actually, semicolon+space) is the standard separator for files in the
`file` field and in this way, you can use the same separator to
distinguish multiple URLs in the `url` field.

By default Ebib also regards everything that is enclosed in a LaTeX
`\url{...}` command as a URL. So if you use `;` to separate URLs and
then happen to run into a URL that contains a semicolon, you can enclose
it in `\url{...}` and it will be recognised properly. You can, of
course, customise the regular expression that controls this behaviour.
See the option “Url Regexp” for details.

Similarly, with the key `I` in the index buffer you can send a DOI to a
browser. The DOI must be stored in the `doi` field. Unlike URLs, there
can only be one DOI in this field. The whole contents of the field is
assumed to be the DOI and is sent to the browser, prepended with the
string `https://dx.doi.org/` if necessary.

Ebib uses the Emacs function `browse-url` to call the default browser on
the system. If you prefer to use another browser, however, you can
specify this with the option “Browser Command”.

# Viewing and Importing Files

If you have electronic versions of the papers in your database stored on
your computer, or any other file associated with your entries (e.g.,
notes, if you store those in separate files) you can use Ebib to call
external viewers for these files or have them opened in Emacs. The
interface for this is similar to that for calling a browser: if you
press `f` in the index buffer, Ebib searches the `file` field for a
filename and when it finds one, calls an appropriate viewer. In the
entry buffer, you can use `f` on any field and it will check that
particular field for a file name. It is also possible to have more than
one filename in a field. In that case, Ebib asks you which one you want
to open.

The file names in the `file` field do not have to have full paths. You
can set the option “File Search Dirs” to a list of directories that Ebib
should search when opening a file from the `file` field. Note that Ebib
searches only the directories in this list, not their subdirectories.
However, you can specify a relative path in the `file` field: if you put
something like `a/Abney1987.pdf` in the `file` field, Ebib searches for
the relevant file in a subdirectory `a/` of the directories listed in
the option “File Search Dirs”. As an extra service, Ebib also searches
for the base filename, i.e., `Abney1987` in this particular case.

Ebib can call different external programs depending on the file
extension of the relevant file. The option “File Associations” allows
you to specify which programs to call for which types of files. By
default, `.pdf` and `.ps` files are handled, by `xpdf` and `gv`,
respectively. You can specify further file types by their extensions (do
not include the dot). The program is searched for in `PATH`, but you can
of course specify the full path to the program. You can also specify
further command line arguments, but if you do this, you should include
the directive `%s` in the string, which will be replaced with the full
path to the file you are opening. (There is no need to put `%s` in
double quotes in order to handle possible spaces in the file name, Ebib
takes care of this.)

There is also the option to open files in Emacs. Use this if you want to
read pdf files in Emacs, for example, with `doc-view-mode` or
[`pdf-tools`](https://github.com/politza/pdf-tools). In the
customisation buffer, you can choose the option “Open in Emacs”, in your
init file (the variable is called `ebib-file-associations`), you can
simply leave the file association empty, or remove the relevant entry
entirely. If Ebib doesn’t find a program to use for a specific file
type, it opens the relevant file in Emacs.

If the `file` field is empty, pressing `f` causes Ebib to search for a
pdf file with a name based on the entry key. By default, Ebib just
appends `.pdf` to the entry key and tries to find a file by the name
thus created. If you want, you can modify the file name that Ebib
searches for by setting the option `ebib-name-transform-function` to a
function that performs the transformation. This function takes the key
of the current entry as its argument (as a string), and should return
the file name to use (without `.pdf`, which is added automatically).
Note that you can use the function `ebib-get-field-value` to access the
values of the entry’s fields (you need to pass `ebib--cur-db` for the
`db` argument).

There are two functions that can help you to attach files to your
database: `ebib-download-url` and `ebib-import-file`. By default, these
are not bound to any keys, but they can of course be called with `M-x`.
The first of these, `ebib-download-url` attempts to convert the URL in
the `url` field into a URL that points to a pdf file, downloads that
file, renames it and saves it in the first directory in
`ebib-file-search-dirs`. The name under which the file is saved is
created by applying the function in `ebib-name-transform-function` to
the entry key and adding `.pdf` to it.

How a URL should be converted to a URL pointing to the pdf file depends
on several factors, of course. The option
`ebib-url-download-transformations` is used to decide how to convert a
particular URL. Currently, only three internet archives are supported:
[arXiv](https://arxiv.org/), [lingBuzz](https://ling.auf.net/lingBuzz/)
and [JSTOR](https://www.jstor.org/). Suggestions for other sites are of
course welcome.

The function `ebib-import-file` can be used to import a file into the
database that is stored on your computer somewhere. It asks for the file
name, renames the file and moves it to the first directory in
`ebib-file-search-dirs`. The file name is created in the same way as
with `ebib-download-url`: by applying the function in
`ebib-name-transform-function` to the entry key. The extension of the
original file is maintained, however, so it doesn’t just work for pdf
files.

Both `ebib-download-url` and `ebib-import-file` add the imported file to
the `file` field if it is not already there.

## Editing the `file` field

As mentioned above, editing the `file` field is a bit different from
editing other fields. Instead of typing the full contents of the file
field, you are asked to specify a single file name. When you hit `RET`,
Ebib adds the filename to the `file` field, appending it to any existing
contents (adding a separator if necessary), and then asks you for the
next file. If you don’t want to add another, just hit `RET`. The default
separator is `"; "` (semicolon-space), but this can be customised (see
the option “Filename Separator” for details). The advantage of this
method is that you can use `TAB` completion to complete file names.

The first directory in the option “File Search Dirs” is used as the
starting directory for filename completion when editing the `file`
field. Note that when completing file names, Ebib does not take the
directories in “File Search Dirs” into account: completion is done using
the standard Emacs file name completion mechanism. However, when you
enter a file name, Ebib checks if it is in a (subdirectory of) one of
the directories in “File Search Dirs”, and if so, cuts off the relevant
part of the file name to turn it into a relative path. (You can disable
this behaviour with the option `ebib-truncate-file-names`: if unset,
file names are always stored as absolute paths.)

# Notes files

Ebib supports the `annotation` field (or `annote` field in BibTeX), but
if you prefer to keep notes outside the `.bib` file, there is an easy
way to do that as well. When you hit `N` on an entry in the index
buffer, Ebib creates a note for the entry, which is saved in a separate
file. If an entry already has a note associated with it, `N` opens it.
The mode line of the entry buffer indicates whether an entry has a note
associated with it by displaying the string `[N]` (customisable with
`ebib-notes-symbol`). By default, each note is saved to its own file,
but you can also use a single file to store all notes.

## Separate notes files

If you wish to use separate files for each note, you need to configure
the directory in which to store them by setting the option
`ebib-notes-directory`. If this is not set, Ebib uses the first
directory in `ebib-file-search-dirs`, (which defaults to the user’s home
directory).

The name of a notes file is formed by taking the entry’s key and
appending the extension `.org` to it, which means that a notes file is
(by default) an Org file. Before creating the file name, Ebib applies
the function in `ebib-notes-name-transform-function` to it, or, if this
is not set, the function in `ebib-name-transform-function`. See [Viewing
Files](#viewing-and-importing-files) for some examples of the changes
that can be applied. Note that if you do not wish to apply any changes
but also do not want the function in `ebib-name-transform-function` to
be applied, you can set `ebib-notes-name-transform-function` to
`identity`.

When a new note is created, it is given a title (an Org headline)
consisting of the author(s), year and title of the entry. Ebib also
includes a `:PROPERTIES:` block containing a custom ID for the entry,
which consists of the entry key. This initial contents is based on a
template, which can be customised.

As mentioned, notes files are Org files by default. This can be changed
by customising the option `ebib-notes-file-extension`. If you change
this option, it makes sense to change `ebib-notes-template` as well,
since the template is an Org template. How to customise the template is
discussed below.

## One single notes file

If you wish to store all notes in a single file, you must set the option
`ebib-notes-file` to the notes file. In this case, the options
`ebib-notes-directory` and `ebib-notes-extension` are ignored, which
means that you must specify the full path and the extension of the notes
file. The option `ebib-notes-name-transform-function` is also ignored.

Ebib assumes that the notes file is an Org file and creates notes using
the template in `ebib-notes-template`, but the Org format is not
enforced. If you specify a notes file with an extension different from
`.org`, the corresponding format will be used, which requires
customising the template. How to do this is discussed below.

There are three hooks that can be used to change the way the notes file
is displayed when a note is opened. When an existing note is displayed,
the hook `ebib-notes-open-note-after-hook` is run. By default, this
contains two functions: `org-back-to-header`, which puts point at the
start of the note, and `org-narrow-to-subtree`, which narrows the notes
buffer to just the note you’re viewing.

When a new note is created, the hook `ebib-notes-new-note-hook` is run.
By default, this contains the function `org-narrow-to-subtree`. For a
new note, the cursor is positioned after the title and the
`:PROPERTIES:` block, so that you can start typing right away.

Because both these hooks narrow the notes buffer, there must be a way to
widen the buffer again when searching for another note. This is the
purpose of `ebib-notes-search-note-before-hook`. This hook is run every
time Ebib searches a note (to see if it exists or to open it) and by
default contains the function `widen`, so that the entire buffer is
searched.

All three hooks are customisable. For example, if you prefer not to
narrow the buffer, simply remove the corresponding functions from the
hooks.

## Customising the notes file format

If you do not want to use Org mode to write your notes files, you can do
so, but several things have to be configured. Firstly, you will need to
customise the template that is used to create a new note. By default,
this template creates an Org entry whose header consists of the author
or editor, the year of publication and the title. The entry has a
`:PROPERTIES:` block containing a `Custom_id:`.

To see how the template can be customised, it is easiest to look at the
default template first:

    "* %T
    :PROPERTIES:
    %K
    :END:
    >|<
    "

This template contains two format specifiers: `%K` and `%T`. `%K` is
replaced with the key of the entry prepended with the string
`"Custom_id: "` in order to create an Org property. The `%T` specifier
is replaced with the title of the note, which consists of the author (or
editor), the year of publication and the title of the bibliography
entry. The template also contains the string `">|<"`, which indicates
the position of the cursor when a new note is created.

It is possible to change the template by customising the option
`ebib-notes-template`. If you use Org for your notes and keep your notes
in a single file, the template **must** contain a `:PROPERTIES:` block
with the `%K` format specifier, because it is required in order to
identify the note and connect it to its BibTeX entry. Without it, Ebib
won’t be able to tell whether an entry has a note or not.

If you use a separate file for each note, the notes are identified by
the file name, so there’s no real need for the `:PROPERTIES:` block, but
it can still be useful if you use other Org-based tools on your note
files.

If you do not wish to use Org mode for your notes, it is easiest to use
separate note files, but as long as you have a `%K` directive in your
template (and an appropriately defined function for it, see
`ebib-notes-template-specifiers` below), it should still be possible to
use a single notes file. (Emphasis on *should*, however, because this
has not been tested.)

There are a few more specifiers that may be used in the template: `%F`
creates an Org link to the file in the BibTeX entry’s `file` field, `%D`
creates an Org link to the DOI in the entry’s `doi` field, and `%U` an
Org link to the entry’s `url` field. There is also a `%L` specifier,
which creates an Org link to the entry’s file, its DOI, or its URL,
whichever is found first.

It is possible to change the strings that the specifiers produce, or to
add new specifiers, by customising the option
`ebib-notes-template-specifiers`. This option contains pairs of
characters and functions. Each function takes two arguments, `key` and
`db`, the key of the entry for which a note is created and the database
in which it is stored. It should return a string (possibly empty), which
replaces the specifier in the template. In order to change the string
that a specifier is replaced with, write your own function and set
`ebib-notes-template-specifiers` to use it.

When the specifier functions are called, the `key` argument is set to
the key of the current entry and the `db` argument to the current
database. With these arguments, it is possible to, e.g., retrieve the
value of a specific field in the entry:

    (ebib-get-field-value <field> key db 'noerror 'unbraced 'xref)

where `<field>` is the field (as a string) whose value is to be
retrieved.

Instead of a function, you may also provide a variable. The variable’s
value is then used to replace the specifier.

## Displaying notes

If an entry has an external note, the first few lines are shown in the
entry buffer as a field called `external note`. The number of lines to
show can be customised with the option `ebib-notes-display-max-lines`,
which defaults to 10. If you prefer, you can also have the entire note
shown, not just the first few lines, by customising the option
`ebib-notes-show-note-method`. The note is then shown in a separate
buffer that is displayed when an entry has a note. This setting is only
really convenient if you use a single notes file, because the buffer is
not closed after displaying the note. If you use separate files for each
note, you’ll end up with a lot of open buffers. (Showing only the first
few lines in the entry buffer does not have this limitation, as it just
reads the text of the note from the file, it does not visit the file in
a buffer.)

Note also displaying the note inline in the entry buffer is only
possible with Org files, so your notes must use Org mode for it to work.
Showing the entire note in a separate buffer can be done with any
format, but only works if you use Ebib’s default window layout (see the
section [Window Management](#window-management) for details), because
that is the only window layout that ensures that the note can be
displayed without getting in the way.

# Managing a reading list

Ebib offers the ability to manage a reading list as an Org file. In
order to make use of this functionality, you must set the option
`ebib-reading-list` to a file in which the reading list is stored. Once
you’ve specified a file, you can add the current entry to the reading
list with `R a`. The mode line of the entry buffer will show `[R]` to
indicate that the current entry is on the reading list.

A reading list is simply an Org file with one entry (i.e., heading) per
item. Each entry is marked with `TODO`, so that the items can be
included in the Org agenda. If you prefer to use another todo state, you
can customise the option `ebib-reading-list-todo-marker`. You can mark
an entry as done from within Ebib with the key `R d`. This will change
the todo state of the item to `DONE` (customisable through the option
`ebib-reading-list-done-marker`). With `R v` you can view the reading
list.

The format of a reading list item can be customised in much the same way
that notes are. The default template for reading list items is provided
by the option `ebib-reading-list-template`, and the specifiers that can
be used in this template are in `ebib-reading-list-template-specifiers`.
Most of the specifiers are the same as for the notes template, with the
exception of `%K`. For the reading list, this specifier uses a different
function, which adds a prefix `reading_` to the key. In this way, the
custom ID of a reading list item and a note will not interfere.
Furthermore, the reading list template accepts a specifier `%M`, which
is replaced with the todo marker specified in the option
`ebib-reading-list-todo-marker` (by default `TODO`).

Most aspects of the reading list can be customised. First, the option
`ebib-reading-list-add-item-function` holds a function that places point
where the new item should be inserted. By default, it puts point at the
end of the buffer. Second, `ebib-reading-list-remove-item-function`
holds the function that marks a reading list item as done. By default,
it is set to `ebib-reading-list-mark-item-as-done`, which simply changes
the todo state of the item to `DONE`, but you can set it to a function
that does something else (for example, completely removing the entry
from the list).

The option `ebib-reading-list-item-active-function` holds a function
that should return `t` if the current entry is on the reading list and
is still active. The default function simply checks if the entry’s todo
state is equal to `ebib-reading-list-todo-marker`.

Lastly, there are two hooks, `ebib-reading-list-new-item-hook` and
`ebib-reading-list-remove-item-hook`. The former is run immediately
after a new reading list item is inserted in the reading list file (but
before saving it), the latter immediately after calling the function in
`ebib-reading-list-remove-item-function` (also before saving the
buffer). By default, these hooks are empty.

# Window Management

By default, Ebib takes over the entire Emacs frame it is started in,
displaying the index window at the top and the entry window below it.
There are a few options to change this behaviour, however. They are all
part of the customisation group `ebib-windows`, and allow you to specify
two alternative ways to deal with Ebib windows. The main layout option
is simply called “Layout” and has four options: use the full frame (the
default), use the current window, use the right part of the frame, or
display only the index window.

If you set the layout to use only the right part of the frame, the Ebib
buffers are displayed on the right of the frame, with the (usually
larger) left part of the frame displaying some other buffer, normally
the buffer from which you called Ebib. The width of the Ebib windows can
be set with the option “Width”, which defaults to 80, and which can be
specified as an absolute value (the number of columns), but also as a
value relative to the current window. In that case, you must specify a
value between 0 and 1. Note that when this option is used, the key `z`
does not hide the Ebib buffers, it simply switches to a non-Ebib window
in the same frame. You can use (uppercase) `Z` to hide the Ebib buffers.
Furthermore, with this option, the multiline edit buffer is not
displayed in the same window as the entry buffer. Rather, Ebib uses
another, non-Ebib window to display it.

The fourth option that Ebib provides is to only show the index buffer on
start-up. In this case, Ebib does not display the entry buffer when it
is started. Instead, only the index buffer is displayed, which can be
navigated in the usual manner. The entry buffer is only displayed when
you add or edit an entry. When you’ve finished editing and move back to
the index buffer, the entry buffer is hidden again.

The entry buffer is also displayed if you press `RET`. When you do this,
the index buffer remains selected, so you can use this to display the
fields of an entry without moving focus to the entry window. If you
navigate the index buffer, the entry buffer remains visible, updating
its contents as you move around.

In this case, too, the key `z` does not hide the index window. Rather,
it just selects another, non-Ebib window. In order to hide the index
window, you can use (uppercase) `Z`.

If you set Ebib’s layout to display only the index buffer on startup,
you can additionally set the option “Popup Entry Window”. Normally, Ebib
will reuse an existing window to display the entry buffer (and restore
its original buffer when you leave the entry buffer). With this option
set, however, Ebib uses the Emacs function `display-buffer-popup-window`
to create a new window (which is destroyed again when you leave the
entry buffer).

Further relevant options are “Window Vertical Split”, which displays the
index buffer to the left of the frame rather than at the top, and “Index
Window Size”, which determines the size of the index window (either its
height or its width, depending on whether the index window is displayed
at the top or on the left of the frame.)

# Creating Entry Stubs

If you have a directory full of (pdf) files of articles that you want to
add to your database, Ebib can make the task a little bit easier by
creating entry stubs for all the files. You can do this with the command
`M-x ebib-add-file-entry`. This command asks you for a file or a
directory and creates an entry in the current database for that file or
each file in the directory. The entries only contain a file field
pointing to the file, all the other information still has to be filled
out by hand, but this way you can at least keep track of which files are
already in your database.

Note that the entry keys for the stubs are temporary keys. They will be
replaced by more permanent keys automatically when you edit the entries.
This behaviour is controlled by the function `bibtex-generate-autokey`,
which has a number of customisation options. Check out its doc string
for details. If you prefer to edit the keys by hand, you can do so by
pressing `E` in the index buffer.

# @Preamble Definition

Apart from database entries, BibTeX allows three more types of elements
to appear in a `.bib` file. These are `@Comment`, `@Preamble` and
`@String` definitions. Ebib provides facilities to handle these, which
are discussed here and in the following sections.

Ebib allows you to add one `@Preamble` definition to the database. In
principle, BibTeX allows more than one such definition, but really one
suffices, because you can use the concatenation character `#` to include
multiple TeX or LaTeX commands. So, rather than having two `@Preamble`
definitions such as:

    @Preamble{ "\newcommand{\noopsort}[1]{} " }
    @Preamble{ "\newcommand{\singleletter}[1]{#1} " }

you can write this in your `.bib` file:

    @Preamble{ "\newcommand{\noopsort}[1]{} "
             # "\newcommand{\singleletter}[1]{#1} " }

Creating or editing a `@Preamble` definition in Ebib is done by hitting
(uppercase) `P` in the index buffer. Ebib uses the multiline edit buffer
for editing the text of the `@Preamble` definition, which means that
`C-c | q` stores the `@Preamble` text and returns focus to the index
buffer, while `C-c | c` returns focus to the index buffer while
abandoning any changes you may have made. (For details on using
multiline edit buffers, see [Multiline Edit
Buffers](#multiline-edit-buffers).)

In order to create a `@Preamble` as shown above in Ebib, you only have
to type the text between the braces. Ebib takes care of including the
braces of the `@Preamble` command, but otherwise it saves the text
exactly as you enter it. So in order to get the preamble above, you’d
have to type the following in Ebib:

    "\newcommand{\noopsort}[1]{} " # "\newcommand{\singleletter}[1]{#1} "

Note that when Ebib loads a `.bib` file that contains more than one
`@Preamble` definition, it concatenates all the strings in them in the
manner just described and saves them in one `@Preamble` definition.

# @String Definitions

If you press (uppercase) `S` in the index buffer, Ebib hides the entry
buffer in the lower window and replaces it with the *strings buffer*. In
this buffer, you can add, delete and edit `@String` definitions.

Adding a `@String` definition is done with the command `a`. This will
first ask you for an abbreviation and then for the value to be
associated with that abbreviation. Once you’ve entered these, Ebib will
sort the new abbreviation into the buffer.

Moving between the `@String` definitions can be done in the usual way:
the cursor keys `up` and `down`, `p` and `n` or `C-p` and `C-n` move up
and down. `Space` and `PgDn` move ten strings down, while `b` and `PgUp`
move in the other direction. The keys `g`, `G`, `Home` and `End` also
function as expected.

To delete a `@String` definition, use `d`. To edit the value of a
definition, use `e`. There is also a command `c`, which copies the value
of the current `@String` definition to the kill ring. Unlike in the
entry buffer, there are no corresponing commands `y` and `x`. (In fact,
`x` does exist, but has another function.) Yanking from the kill ring
can be done with `C-y/M-y` in the minibuffer when you edit a `@String`’s
value. Cutting a `@String`’s value is pointless, because a `@String`
definition must have a value.

Having defined `@String` definitions, there must of course be a way to
use them. Just giving a field a string abbreviation as value will not
do, because Ebib puts braces around the value that you enter when it
writes the `.bib` file, so that BibTeX will not recognise the
abbreviation, and will not expand it. BibTeX will only recognise an
abbreviation if it appears in the `.bib` file outside of any braces.

To accomplish this, you must mark a field’s value as special. A special
field is a field whose value is not surrounded by braces when the
database is saved, so that BibTeX recognises it as an abbreviation. To
mark a field special, press `r`. An asterisk will appear before the
field, indicating that has no braces. Pressing `r` again will change the
field back to normal. If you press `r` on a field that does not have a
value yet, Ebib will ask you for one.

Note that this also makes it possible to enter field values that are
composed of concatenations of strings and abbreviations. The BibTeX
documentation for example explains that if you have defined:

    @String{WGA = "World Gnus Almanac"}

you can create a BibTeX field like this:

    title = 1966 # WGA

which will produce “1966 World Gnus Almanac”. Or you can do:

    month = "1~" # jan

which will produce someting like “1 January”, assuming your bibliography
style has defined the abbreviation `jan`. All this is possible with
Ebib, simply by entering the exact text including quotes or braces
around the strings, and marking the relevant field as special.

An easy way to enter a `@String` abbreviation as a field value is to use
the key `s` instead of `e`. If you type `s`, Ebib asks you for a
`@String` abbreviation to put in the current field, and automatically
marks the field as special. With this command, Ebib only accepts
`@String` definitions that are in the database, so that by using `s` you
can make sure you don’t make any typos. Note that you can use `TAB`
completion to complete a partial string.

# @Comments

If Ebib finds a `@Comment` in a `.bib` file, it will read it and store
it in the database. When the database is saved, all the `@Comment`s will
be saved with it, at the top of the file, immediately after the
`@Preamble` (with the exception of a `@Comment` surrounding a `Local
Variables:` block, which is saved at the end of the file). There is no
way to edit comments, nor can you specify where in the `.bib` file a
comment is placed, but they won’t be lost.

# Managing Keywords

Biblatex supports a `keywords` field, which can contain a
(comma-separated) list of keywords for an entry. BibTeX does not support
this field directly, but Ebib includes a `keywords` field in the extra
fields for BibTeX entries. Ebib offers some special facilities for
editing this field.

Ebib keeps a list of keywords used in your database(s) and offers these
for completion when you edit the `keywords` field. You can enter a
keyword and accept it with `RET`, after which you will be asked for the
next keyword. Just hitting `RET` without any input finishes the edit and
returns focus to the entry buffer. (With selectrum, ivy or helm the key
binding to finish editing the `keywords` field is different; the prompt
will indicate what key to press). You can, of course, also enter a
keyword that is not on the completion list. If you do, it will be added
to the list.

If you need to edit a keyword or remove one from the list, you need to
edit the `keywords` field directly. To do this, use a prefix argument:
`C-u RET` instead of just `RET` to edit the field. Note, though, that
this does not update the completion list.

The keywords completion list is composed of the keywords in all the
`.bib` files you have open and is available in every database. If you
open another `.bib` file, its keywords are added to the completion list.
(Note that if you close a database, its keywords are not removed from
the completion list, since Ebib does not keep track of which keywords
are used in which database.)

## Using a Canonical Keywords List

By default, the completion list does not contain keywords that are not
used in any of your `.bib` files. If you wish to use a set of canonical
keywords that are always offered for completion, regardless of whether
they are used in a currently opened `.bib` file or not, you can set the
option “Keywords” (`ebib-keywords`). This can be a list of keywords or
the name of a file containing the keywords. If it is a file name, the
file should be a simple text file with one keyword per line.

If you set this option, keywords in a database that are not in the
canonical list are displayed in Ebib’s warning face
(`ebib-warning-face`). You can add them to the canonical list with the
key sequence `K s`, which will ask you for the keyword to add, or with
`K c`, which adds all keywords of the current entry to the canonical
list. You can also remove all keywords from the `keywords` field that
are not in the list of canonical keywords with the key sequence `K p`.

Even if you use a list of canonical keywords, you can still enter
keywords that are not in the list when you edit the `keywords` field. If
you do so, the new keywords are added to the list automatically. If you
do not wish this to happen, unset the option “Keywords Add New To
Canonical”.

If new keywords were added to the list of canonical keywords, you will
be asked if you wish to save the list when you quit Ebib. If you always
want this to happen without asking for confirmation, set the option
“Keywords Save On Exit” to `always`. Note that you can also save the
list manually with the key sequence `K S` (capital `K`, capital `S`).

If you haven’t configured a list of canonical keywords, the key sequence
`K S` creates one from the keywords used in your open `.bib` files. The
list is then saved to your customisation file (usually
`~/.emacs.d/init.el`). If you prefer to keep your keywords in a separate
file, you need to create the file yourself (as mentioned, one keyword
per line; keywords may of course contain spaces), and configure the
option “Keywords” (`ebib-keywords`) yourself.

# Sorting the `.bib` File

By default, the entries in the database are saved to the `.bib` file in
alphabetical order according to entry key. If you only deal with the
`.bib` file through Ebib, you may not care in which order the entries
are saved. However, it may sometimes be desirable to be able to specify
the sort order of entries in more detail. (Apparently, this can be
useful with ConTeXt, for example.)

You can specify a sort order in Ebib’s customisation buffer. To sort the
entries, you must set at least one sort level (that is, a field to sort
the entries on). You can also specify more than one sort level: if two
entries have identical values for the first sort level, they will be
sorted on the second sort level. E.g., if the first sort level is
`author` and the second is `year`, then the entries are sorted by
author, and those entries that have identical values for the `author`
field are sorted by year.

A sort level is not restricted to a single field. You can specify more
fields for a single sort level. Within a single sort level, a second
sort field is used if the first sort field does not have a value. For
example, books that have an editor instead of an author will have an
empty `author` field. If you sort the database on the `author` field,
such entries will all appear at the beginning of the `.bib` file, which
is most likely not what you want.

To remedy this, you can specify both the `author` and the `editor`
fields for the first sort level. Ebib will then sort an entry on its
`author` field if it has a value, and will otherwise use the value of
the `editor` field.

The difference between two sort fields within one sort level and two
sort levels is that a second sort *field* is an alternative for the
first field when it has no value, while a second sort *level* is an
additional sort criterion when two or more entries cannot be sorted on
the first level, because they have identical values.

By default, the option `Sort Order` has no value, which means that the
entries in the `.bib` file are sorted according to entry key. Those that
wish to customise the sort order will usually want to set the first sort
level to `author editor`, and the second to `year`. In that way, the
entries in the `.bib` file are sorted according to author/editor, and
entries with the same author/editor are sorted by year.

Entries that cannot be sorted on some sort level, because the sort
fields are empty, are sorted on entry key. (Keep in mind that if the
first sort level yields *no value* for a specific entry, Ebib does *not*
use the second sort level to sort that entry. It uses the entry key. The
second sort level is only used if the first yields *identical* values
for two or more entries.)

Note that if you wish to make use of this option, you need to unset the
option “Save Xrefs First” ([Cross-referencing](#cross-referencing)). It
is pointless to set a sort order if cross-referenced entries are saved
first. Since “Save Xrefs First” is set by default, you need to unset if
you set “Sort Order”.

# Merging and Importing

As described in the previous chapter, adding entries to a database can
be done manually with the key `a`. There are other ways of adding
entries to a database, however.

In the index buffer, the Ebib menu has an option to merge a second
`.bib` file into the current database. Ebib reads the entries in this
file and adds them to the database. Duplicate entries (that is, entries
with an entry key that already exists in the database) will normally not
be loaded. Ebib logs a warning about each duplicate entry to its log
buffer and displays a warning after loading the `.bib` file when this
happens. However, if you’ve customised Ebib to automatically generate
keys, duplicate entries will be merged into the current database under a
unique key.

Another way to add entries to a database is to import them from an Emacs
buffer. If, for example, you find ready-formatted BibTeX entries in a
text file or on the internet, you can copy & paste them to any Emacs
buffer (e.g. the `*scratch*` buffer), and then execute the command `M-x
ebib-import`. Ebib then goes through the buffer and loads all BibTeX
entries it finds into the current database (i.e. the database that was
active when you lowered Ebib). If you call `ebib-import` while the
region is active, Ebib only reads the BibTeX entries in the region.

If a BibTeX entry in the buffer lacks an entry key (which sometimes
happens with BibTeX entries found on the internet), Ebib will generate a
temporary key for it of the form `<new-entryXX>`, where `XX` is a
number. You can change such keys by hitting `E` in the index buffer.
They will also automatically be replaced with a more sensible key when
you edit them. See the option “Autogenerate Keys” for details.

# Exporting Entries

Sometimes it can be useful to copy entries from one database to another,
or to create a new `.bib` file with several entries from an existing
database. For this purpose, Ebib provides exporting facilities. To
export an entry to another database, use the command `x`. This command
operates on a single entry or on all marked entries. Ebib will ask you
for the database to export the entry or entries to. `TAB` completion is
available, based on the file names of the databases.

You can also export entries to a file. To do this, call the command `x`
with a prefix argument: `C-u x`. You will be prompted for the file name
to export the entries to. If the file already exists, Ebib appends the
entries to it. Note that in this case, there is no check to see if the
exported entries already exist in the target file, so it’s possible to
end up with duplicate entries in this way.

Apart from entries, it is also possible to export the `@Preamble` and
`@String` definitions. The `@Preamble` definition is exported with the
command `X` in the index buffer. `@String` definitions can be exported
in the strings buffer: `x` in this buffer exports the current string,
while `X` exports all `@String` definitions in one go. All these
commands function in the same way: when used without a prefix argument,
they ask for an open database to export the entry to. With a prefix
argument, they ask for a filename, and then append the relevant data to
that file.

# Multiple Identical Fields

Under normal circumstances, a BibTeX entry only contains one occurrence
of each field. If `biblatex` or BibTeX notices that an entry contains
more than one occurrence of a required or optional field, it issues a
warning. Ebib is somewhat less gracious, it simply takes the value of
the last occurrence without giving any warning. (Note, by the way, that
`biblatex` will use the value of the *first* occurrence, not the last.)
When extra fields appear more than once in an entry, `biblatex` does not
warn you, since it ignores those fields anyway. Here, too, Ebib’s
standard behaviour is to ignore all but the last value.

However, some online reference management services “use” this feature of
BibTeX in that they put multiple `keywords` fields in the BibTeX entries
that they produce. If you were to import such an entry into Ebib, you
would lose all your keywords except the last one. To remedy this, you
can tell Ebib that it should allow multiple occurrences of a single
field in a BibTeX entry. You can do this by setting the customisation
option “Allow Identical Fields”.

With this option set, Ebib collapses the multiple occurrences into a
single occurrence. All the values of the different occurrences are
collected and stored in the single occurrence, separated by the default
keywords separator (`ebib-keywords-separator`). That is, Ebib does not
retain the multiple occurrences, but it does retain the values. So
suppose you have an entry that contains the following `keywords` fields:

    @book{Jones1998,
        author = {Jones, Joan},
        year = {1998},
        ...
        keywords = {sleep},
        keywords = {winter},
        keywords = {hibernation}
    }

If you load this entry into Ebib with the option “Allow Identical
Fields” set, you will get the following:

    @book{Jones1998,
        author = {Jones, Joan},
        year = {1998},
        ...
        keywords = {sleep, winter, hibernation}
    }

# Multiline Edit Buffers

As mentioned several times before, field values that contain newlines
(so-called *multiline fields*) and the `@Preamble` are edited in a
so-called *multiline edit buffer*. This section discusses the details of
this buffer.

Ebib enters a multiline edit buffer in one of three cases: when you edit
the `@Preamble` definition, when you hit `m` in the entry buffer to edit
the current field as multiline, or when you hit `e` on the
`annote`/`annotation` or `abstract` fields, or on a field whose value
already is multiline.

The major mode that is used in multiline edit buffers is
user-configurable. The default value is `text-mode`, but if you prefer
to use some other mode, you can specify this through the customisation
option `ebib-multiline-major-mode`.

Three commands are relevant for interacting with Ebib when you’re in the
multiline edit buffer, which are bound to key sequences in the minor
mode `ebib-multiline-edit-mode`, which is activated automatically in the
multiline edit buffer.

`ebib-quit-multiline-buffer-and-save`, bound to `C-c | q`, leaves the
multiline edit buffer and stores the text in the database. If you invoke
this command when you’ve deleted all contents of the buffer (including
the final newline\!) and you were editing a field value or the
`@Preamble`, the field value or preamble is deleted. (This is in fact
the *only* way to delete the `@Preamble` definition. Field values on the
other hand can also be deleted by hitting `k` or `d` on them in the
entry buffer.)

`ebib-cancel-multiline-buffer`, bound to `C-c | c`, also leaves the
multiline edit buffer, but it does so without storing the text. The
original value of the field, string or preamble will be retained. If the
text was modified, Ebib will ask for a confirmation before leaving the
buffer.

`ebib-save-from-multiline-buffer`, bound to `C-c | s`, can be used in
the multiline edit buffer to save the database. This command first
stores the text in the database and then saves it. Because Ebib does not
do an autosave of the current database, it is advisable to save the
database manually every now and then to prevent data loss in case of
crashes. It would be annoying to have to leave the multiline edit buffer
every time you want to do this, so this command has been provided to
allow you to do this from within the buffer.

Note that you do not need to finish a multiline edit before you can
return to the database and possibly edit other fields and even entries.
Ebib keeps track of which field in which entry of which database a
multiline edit buffer belongs to, so you can keep a multiline edit
buffer open while doing other work. It is even possible to have several
multiline edit buffers open at the same time. Ebib makes sure that when
you finish one, its contents is stored in the correct place.

Admittedly, the key combinations of the multiline edit buffer are
somewhat awkward. The reason for this is that these commands are part of
a minor mode, which restricts the available keys to combinations of
`C-c` plus a non-alphanumeric character. However, it is possible to
change the key commands, if you wish. For example, you could put
something like the following in your `~/.emacs`:

``` commonlisp
(with-eval-after-load 'ebib
  (define-key ebib-multiline-mode-map
    "\C-c\C-c" 'ebib-quit-multiline-buffer-and-save)
  (define-key ebib-multiline-mode-map
    "\C-c\C-q" 'ebib-cancel-multiline-buffer)
  (define-key ebib-multiline-mode-map
    "\C-c\C-s" 'ebib-save-from-multiline-buffer))
```

This sets up `C-c C-c`, `C-c C-q` and `C-c C-s` for use in the multiline
edit buffer. Since such key combinations are restricted for use with
major modes, however, Ebib cannot set these up automatically, but as an
Emacs user, you are free to do as you like, of course.

# The Options Menu

In the index buffer, Ebib’s menu has an Options submenu. This menu gives
you quick access to Ebib’s customisation buffer, and it also provides
checkboxes for several settings that can be toggled on and off. All of
these settings have defaults that can be defined in the customisation
buffer. Setting or unsetting them in the Options menu only changes them
for the duration of your Emacs session, it doesn’t affect the default
setting.

The same is true for the printing options that are in the Print menu.
When set or unset in the menu, the default values specified in the
customisation buffer do not change.

# Customisation

Ebib can be customised through Emacs’ standard customisation interface.
The relevant customisation group is (obviously) called `ebib`, which has
five subgroups: `ebib-faces`, `ebib-filters`, `ebib-notes`, and
`ebib-keywords`, whose functions should be obvious, and `ebib-windows`,
where options for Ebib’s window management can be set. All options are
documented in the customisation buffers. You can go to Ebib’s
customisation buffer with `M-x customize-group RET ebib RET`, or by
using the menu «Ebib | Options | Customize Ebib».

## Modifying Key Bindings

If you would like to change Ebib’s standard key bindings, or if you
would like to bind a command that is only available through the menu to
a key, you can do so by adding the relevant key bindings to Emacs init
file (`~.emacs.d/init.el` by default). The relevant key maps are
`ebib-index-mode-map`, `ebib-entry-mode-map`, `ebib-strings-mode-map`
for the index, entry, and strings buffer, and `ebib-multiline-mode-map`,
which adds keys to finish writing multiline field values.

In addition, `ebib-search-map` is a transient key map that is activated
when `ebib-search` is called, and `ebib-filters-map`,
`ebib-keywords-map` and `ebib-reading-list-map` are key maps (set up
using `define-prefix-command`) that contain bindings for filters,
keywords and the reading list, respectively. Finally, there is
`ebib-log-mode-map` which is active in Ebib’s log buffer.

As an example, the default keybindings in`ebib-multiline-mode-map`,
which are rather awkward to type, can be redefined as follows:

``` commonlisp
(with-eval-after-load 'ebib
  (define-key ebib-multiline-mode-map
    "\C-c\C-c" 'ebib-quit-multiline-buffer-and-save)
  (define-key ebib-multiline-mode-map
    "\C-c\C-q" 'ebib-cancel-multiline-buffer)
  (define-key ebib-multiline-mode-map
    "\C-c\C-s" 'ebib-save-from-multiline-buffer))
```
