# GNU Emacs Configuration #

This is my configuration for _GNU Emacs_.

This configuration started in November 2010, derived from [mefyl's
configuration][mefylconf].

Mefyl gave an awesome lecture at EPITA in October 5, 2009. Here are
the [slides][mefylslides].

## Resources for Emacs ##

Some interesting places to learn more about Emacs:

* [Emacs wiki][emacswiki]

* [GNU Emacs manual][emacsman]

* [An Introduction to Programming in Emacs Lisp][elispintro]

* [Emacs Lisp Reference Manual][elispman]

* [Other Emacs manuals][otherman]

## Installation ##

1. Grab GNU Emacs (23.* version!), use your package manager for
Linux. For Mac OS X, use a [Universal Binary][emacsformacosx]
(recommended) or [Carbon Emacs][carbonemacs] (a little obsolete now)
and/or [MacPorts][macports] (to update the Terminal version easily).
I dislike [Aquamacs][aquamacs].  Windows users can get it directly
[from GNU][emacswin].

2. Download the latest [tarball][tarball] or [zipball][zipball] of my
configuration.

3. Copy configuration files:

    * On Unix-like systems, you can simply link the directory with `ln
      -s <extracted-dir> ~/.emacs.d`.

    * On Windows, search for the section **3.5 Where do I put my init
      file?** in [GNU Emacs FAQ For MS Windows][emacsfaqwin], and
      follow the instructions (basically put the configuration files
      in an `.emacs.d` directory in your own `Application Data`
      directory).

## Test ##

This configuration is used almost daily under Mac OS X, frequently
under Linux ([Archlinux][archlinux], [Fedora][fedora] and
[Ubuntu][ubuntu]), and occasionally under [FreeBSD][freebsd] and
Windows, for general text editing, and for programming.  In case of
problems, running the last version of everything usually helps.

## COPYING ##

Files are licensed under the same license as Emacs unless otherwise
specified.  See the file COPYING for details.


[mefylconf]: https://github.com/downloads/sillage/emacs/mefyl-emacs-conf.tar.bz2
[mefylslides]: https://github.com/downloads/sillage/emacs/mefyl-emacs-conf.pdf
[emacswiki]: http://www.emacswiki.org/
[emacsman]: http://www.gnu.org/software/emacs/manual/html_node/emacs/index.html
[elispintro]: http://www.gnu.org/software/emacs/emacs-lisp-intro/html_node/index.html
[elispman]: http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html
[otherman]: http://www.gnu.org/software/emacs/manual/index.html
[emacsformacosx]: http://emacsformacosx.com/
[carbonemacs]: http://homepage.mac.com/zenitani/emacs-e.html
[macports]: http://www.macports.org/
[aquamacs]: http://aquamacs.org/
[emacswin]: http://ftp.gnu.org/pub/gnu/emacs/windows/
[tarball]: https://github.com/sillage/emacs/tarball/master
[zipball]: https://github.com/sillage/emacs/zipball/master
[emacsfaqwin]: http://www.gnu.org/software/emacs/windows/
[archlinux]: http://www.archlinux.org/
[fedora]: http://fedoraproject.org/
[ubuntu]: http://www.ubuntu.com/
[freebsd]: http://www.freebsd.org/
