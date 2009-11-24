# Copyright (C) 2007  Xavier Maillard  <xma@gnu.org>
#                     Jose E. Marchesi <jemarch@gnu.org>

# This file is NOT part of GNU Emacs.

# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

# $Id: Makefile,v 1.6 2009/11/24 17:10:25 jemarch Exp $

# make install
# make all
# make clean

EMACS = emacs
SITEFLAG=--no-site-file
GZIP=gzip

SOURCE = uuid.el \
         vcard.el \
         abook.el
TARGET=$(patsubst %.el,%.elc,$(SOURCE))
COMPILED=$(wildcard *.elc)

DESTDIR=
PREFIX=$(DESTDIR)/usr/local
INFODIR=$(PREFIX)/info
MAN1DIR=$(PREFIX)/share/man/man1
SITELISP=$(PREFIX)/share/emacs/site-lisp/abook

INFODIR=$(PREFIX)/info

INSTALLINFO = /usr/sbin/install-info --info-dir=$(INFODIR)

.PHONY: all install clean
.PRECIOUS: %.elc %.info %.html

all: $(TARGET) abook.info

install:
	test -d $(SITELISP) || mkdir -p $(SITELISP)
	[ -d $(INFODIR) ] || install -d $(INFODIR)
	install -m 644 $(SOURCE) $(SITELISP)
	install -m 644 $(COMPILED) $(SITELISP)
	install -m 0644 abook.info $(INFODIR)/abook
	$(INSTALLINFO) abook.info

%.elc: %.el
	@echo "Byte compiling the source file "$<
	@$(EMACS) -batch -q \
		--eval '(setq load-path (cons "." load-path))' \
		-f batch-byte-compile $<

%.info: %.texi
	makeinfo --no-split $<

%.html: %.texi
	makeinfo --html --no-split $<

remove-info:
	$(INSTALLINFO) --remove abook.info

clean:
	-rm -f *~ $(COMPILED) abook.info abook.html
