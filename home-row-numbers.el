;;; -*- lexical-binding: t; -*-

;;; Copyright 2016 Graham Dobbins

;; This program is free software: you can redistribute it and/or modify
;;
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(eval-and-compile (require 'cl))

(defvar home-row-numbers-qwerty
  '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)
  "list of the qwerty home row keys")

(defvar home-row-numbers-dvorak
  '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)
  "list of the dvorak home row keys")

(defvar home-row-numbers-qwerty-numpad
  '(?m ?\, ?\. ?j ?k ?l ?u ?i ?o ?\ )
  "keys forming a numpad under the left hand in qwerty")

(defvar home-row-numbers-dvorak-numpad
  '(?m ?w ?v ?h ?t ?n ?g ?c ?r ?\ )
  "keys forming a numpad under the left hand in dvorak")

(defvar home-row-numbers-norm
  '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
  "list of the numbers on the keyboard in normal order")

(defvar home-row-numbers-zero
  '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "list of the numbers starting with zero")

(defvar home-row-numbers-prog
  '(?7 ?5 ?3 ?1 ?9 ?0 ?2 ?4 ?6 ?8)
  "list of the numbers on the keyboard in programmer dvorak order")

(defvar home-row-numbers-argument-doc
  "Translate the home row keys into digits"
  "doc string for `home-row-numbers-argument")

(defun home-row-numbers-numpad-warning (arg)
  "Issue a warning when ARG is true"
  (when arg
    (warn "home-row-numbers expects the NUMBERS argument to be
    nil when a numpad layout is chosen")))

;;;###autoload
(cl-defmacro home-row-numbers (&key (layout 'qwerty)
				    (message t)
				    (print-key ?p)
				    (numbers nil))
  "Setup `universal-prefix-map' to accept letters as numbers for
use as either a prefix argument or to print into the current
buffer.

The following keywords are understood:

LAYOUT

One of the symbols: qwerty, dvorak, qwerty-numpad, dvorak-numpad.

The first two use the home row of the respective layouts to input
numbers, while the numpad variants use the keys underneath the
left hand's index, middle, and ring fingers on the home row and
the rows above and below plus the space bar to mimic the numpad.
A list of characters can also be provided to be used instead.
Default is qwerty.

MESSAGE

If true the numeric value of `prefix-arg' is printed in the
mini-buffer after each keypress. Default true.

PRINT-KEY

A character to bind `home-row-numbers-print' to. If nil then not
bound. Default ?p.

NUMBERS

One of the symbols: zero-first or programming

The former will move zero to be before one, the latter will
re-order the numbers to be as they are in the programming dvorak
layout. If nil, the default, then order the numbers as they are
on a traditional keyboard layout. Numpad layouts assume this
argument is nil. A list of characters can also be provided to be
used instead."
  (let ((letters (cond
		  ((eql layout 'qwerty) home-row-numbers-qwerty)
		  ((eql layout 'dvorak) home-row-numbers-dvorak)
		  ((eql layout 'qwerty-numpad)
		   (home-row-numbers-numpad-warning numbers)
		   home-row-numbers-qwerty-numpad)
		  ((eql layout 'dvorak-numpad)
		   (home-row-numbers-numpad-warning numbers)
		   home-row-numbers-dvorak-numpad)
		  (t (assert (consp layout)
			     nil
			     "the LAYOUT argument to
		     home-row-numbers should either be a list of
		     characters or one of the symbols specified
		     in the home-row-numbers doc-string")
		     layout)))
	(numbers (cond
		  ((consp numbers) numbers)
		  ((or (eql numbers 'zero)
		       (eql numbers 'zero-first))
		   home-row-numbers-zero)
		  ((or (eql numbers 'programming)
		       (eql numbers 'prog))
		   home-row-numbers-prog)
		  (t home-row-numbers-norm))))
    (assert (= (length letters) (length numbers))
	    nil
	    "the LAYOUT and NUMBERS arguments to home-row-numbers
	    should be the same length")
    `(progn
       (defun home-row-numbers-argument (arg)
	 ,home-row-numbers-argument-doc
	 (interactive "P")
	 (let ((last-command-event
		(case last-command-event
		  ,@(loop for k in letters
			  for n in numbers
			  collect `(,k ,n))
		  (t (user-error
		      "home-row-numbers-argument is not configured for %c"
		      last-command-event)))))
	   (digit-argument arg)
	   ,(when message
	      '(princ
		(format "C-u %d"
			(prefix-numeric-value prefix-arg))))
	   prefix-arg))

       ,@(when print-key
	   `((defun home-row-numbers-print (arg)
	       "Insert `prefix-arg' into the current buffer."
	       (interactive "p")
	       (insert (number-to-string arg)))

	     (define-key universal-argument-map
	       [,print-key] #'home-row-numbers-print)))

       ,@(loop for k in letters
	       collect `(define-key universal-argument-map
			  [,k] #'home-row-numbers-argument)))))

;;;###autoload
(defun home-row-numbers-disable ()
  "Disable home-row-numbers"
  (interactive)
  (substitute-key-definition 'home-row-numbers-argument
			     nil
			     universal-argument-map)
  (substitute-key-definition 'home-row-numbers-print
			     nil
			     universal-argument-map))
