# GNU Emacs Configuration #

This is my configuration for _GNU Emacs_.  It is useful for any EPITA
student and anyone who wants to have an efficient emacs config to code
in C, C++, Java and even [OCaml][].

This configuration started in November 2010, derived from
[mefyl's configuration][mefylconf].  Mefyl is a former ACU 2008, he
gave us an awesome lecture at EPITA on October 5, 2009.  Here are the
[slides][mefylslides].

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
and/or [MacPorts][] (to update the Terminal version easily).  I
dislike [Aquamacs][].  Windows users can get it directly
[from GNU][emacswin].

2. Download and extract the latest [tarball][] or [zipball][] of my
configuration.

3. Copy configuration files:

    * On Unix-like systems, you can simply link the directory with the
      command `ln -s <extracted-dir> ~/.emacs.d`.

    * On Windows, search for the section **3.5 Where do I put my init
      file?** in [GNU Emacs FAQ For MS Windows][emacsfaqwin], and
      follow the instructions (basically put the configuration files
      in an `.emacs.d` directory in your own `Application Data`
      directory).

## Test ##

This configuration is used almost daily under Mac OS X, frequently
under Linux ([Arch Linux][], [Fedora][] and [Ubuntu][]), and
occasionally under [FreeBSD][] and Windows, for general text editing,
and for programming.  In case of problems, running the last version of
everything usually helps.

## COPYING ##

Files are licensed under the same license as Emacs unless otherwise
specified.  See the file COPYING for details.


[OCaml]: http://caml.inria.fr/index.en.html "The Caml language: Home"
[mefylconf]: https://github.com/downloads/sillage/emacs/mefyl-emacs-conf.tar.bz2 "Mefyl's configuration files"
[mefylslides]: https://github.com/downloads/sillage/emacs/mefyl-emacs-conf.pdf "Mefyl's slides"
[emacswiki]: http://www.emacswiki.org/ "Emacs Wiki"
[emacsman]: http://www.gnu.org/software/emacs/manual/html_node/emacs/index.html "GNU Emacs manual"
[elispintro]: http://www.gnu.org/software/emacs/emacs-lisp-intro/html_node/index.html "An Introduction to Programming in Emacs Lisp"
[elispman]: http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html "Emacs Lisp Reference Manual"
[otherman]: http://www.gnu.org/software/emacs/manual/index.html "Other Emacs manuals"
[emacsformacosx]: http://emacsformacosx.com/ "GNU Emacs For Mac OS X"
[carbonemacs]: http://homepage.mac.com/zenitani/emacs-e.html "Carbon Emacs Package"
[MacPorts]: http://www.macports.org/ "The MacPorts Project Official Homepage"
[Aquamacs]: http://aquamacs.org/ "Aquamacs"
[emacswin]: http://ftp.gnu.org/pub/gnu/emacs/windows/ "GNU Emacs FTP"
[tarball]: https://github.com/sillage/emacs/tarball/master "latest tarball"
[zipball]: https://github.com/sillage/emacs/zipball/master "latest zipball"
[emacsfaqwin]: http://www.gnu.org/software/emacs/windows/ "GNU Emacs FAQ For MS Windows"
[Arch Linux]: http://www.archlinux.org/ "Arch Linux"
[Fedora]: http://fedoraproject.org/ "Fedora"
[Ubuntu]: http://www.ubuntu.com/ "Ubuntu"
[FreeBSD]: http://www.freebsd.org/ "FreeBSD"
