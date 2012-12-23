
DESCRIPTION:
===========

Cut down your inbox like a master swordsman.

Extension that turns Emacs into a user friendly email client - as
Magit is to git, elMail is to email.

DEPENDENCIES:
==============

Depends on a Ruby gem, also called Kensei, which in turns acts as glue
and a facade in front of a couple of unix CLI tools: offlineimap (for
synching inboxes) and msmtp (for sending mail).

INSTALLATION:
======

sudo apt-get install offlineimap msmtp
gem install kensei
...
TODO install kensei emacs package


USAGE:
======

M-x kensei-start

HACKING ON KENSEI:
=================

Coding guidelines as reminders for myself and any contributors:

- develop in stock 24.x emacs
- don't load personal elfiles during basic manual/automated testing
- keep it as non-blocking/responsive as possible
- offload any heavy lifting to backend
- don't introduce more external dependencies unless for a good reason

LICENSE:
========

Copyright (C) 2012 Thomas Kjeldahl Nilsson

Authors: thomas@kjeldahlnilsson.net

Keywords: email client extension

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.
