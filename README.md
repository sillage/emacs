# GNU Emacs Configuration #

This is my configuration for _GNU Emacs_.

This configuration started in November 2010, derived from [mefyl's
configuration](https://github.com/downloads/sillage/emacs/emacs-conf.tar.bz2).

Mefyl gave an awesome lecture at EPITA in October 5, 2009. Here are
the
[slides](https://github.com/downloads/sillage/emacs/emacs-conf.pdf).

## Resources for Emacs ##

Some interesting places to learn more about Emacs:

* [Emacs wiki](http://www.emacswiki.org/)

* [GNU Emacs
  manual](http://www.gnu.org/software/emacs/manual/html_node/emacs/index.html)

* [An Introduction to Programming in Emacs
  Lisp](http://www.gnu.org/software/emacs/emacs-lisp-intro/html_node/index.html)

* [Emacs Lisp Reference
  Manual](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html)

* [Other Emacs manuals](http://www.gnu.org/software/emacs/manual/index.html)

## Installation ##

1. Grab GNU Emacs (23.* version!), use your package manager for
Linux. For Mac OS X, use a [Universal
Binary](http://emacsformacosx.com/) (recommended) or [Carbon
Emacs](http://homepage.mac.com/zenitani/emacs-e.html) (a little
obsolete now) and/or [MacPorts](http://www.macports.org/) (to update
the Terminal version easily) (I dislike
[Aquamacs](http://aquamacs.org/)).  Windows users can get it directly
[from GNU](http://ftp.gnu.org/pub/gnu/emacs/windows/).

2. Copy configuration files:

   * On Unix-like systems, you can link the directory with `ln -s
     ~/{emacs_dir} ~/.emacs.d` and the init file with `ln -s
     ~/{emacs.el_file} ~/.emacs`.

   * On Windows, search for the section **3.5 Where do I put my init
     file?** in [GNU Emacs FAQ For MS
     Windows](http://www.gnu.org/software/emacs/windows/), and follow
     the instructions (basically put the configuration files in your
     `Application Data` directory).

If you find yourself missing some autoloads after an update (which
should manifest itself as "void function: foobar" errors) try `M-x
regen-autoloads`. After some updates an `M-x recompile-init` will be
necessary; this should be noted in the commit messages.

## Test ##

This configuration is used almost daily under Mac OS X, frequently
under Linux ([Archlinux](http://www.archlinux.org/),
[Fedora](http://fedoraproject.org/) and
[Ubuntu](http://www.ubuntu.com/)), and occasionally under
[FreeBSD](http://www.freebsd.org/) and Windows, for general text
editing, and for programming. In case of problems, running the last
version of everything usually helps.

## COPYING ##

Files are licensed under the same license as Emacs unless otherwise
specified. See the file COPYING for details.
