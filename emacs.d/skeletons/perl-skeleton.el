(define-skeleton perl-skeleton
  "" ""
  "#!/usr/bin/env perl\n\n"

  "use warnings;\n"
  "use strict;\n"
  "use 5.010;\n\n"

  _
  )

(define-skeleton pod-skeleton
  "" ""
  "=head1 NAME\n\n"

  (let ((path (split-string default-directory "/")))
    (nth (- (length path) 2) path)) _ "\n\n"

  "=head1 SYNOPSIS\n\n"

  "=head1 DESCRIPTION\n\n"

  "=head1 AUTHOR\n\n"

  "=head1 COPYRIGHT\n\n"

  "Wojciech 'vifon' Siewierski <darkvifon at gmail dot com>\n\n"

  "Copyright (C) " (format-time-string "%Y") "  Wojciech Siewierski

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.\n"
  )

(define-auto-insert "\\.pl$" 'perl-skeleton)
(define-auto-insert "\\.pod$" 'pod-skeleton)
