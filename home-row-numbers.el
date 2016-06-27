;;; home-row-numbers.el --- Put numbers on the home row -*- lexical-binding: t; -*-

;;; Copyright 2016 Graham Dobbins

;; Author: Graham Dobbins <gambyte@users.noreply.github.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.5"))
;; Keywords: convenience, home-row, numbers, prefix-arg

;; This program is free software: you can redistribute it and/or modify
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

;;; Commentary:

;; This packages allows for conveniently entering numbers for use with
;; universal arguments or for entering into a buffer. It supports both
;; qwerty and dvorak keyboard layouts and has options for using either
;; the home row or a pseudo numpad type layout. Custom keyboard
;; layouts and number orders are also supported.

(eval-when-compile (require 'cl))

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

(defun home-row-numbers-string->char-list (string)
  "Create a list consisting of the characters of STRING"
  (cl-loop for char across string collect char))

(cl-defmacro home-row-numbers-helper (&key (layout 'qwerty)
					   (message t)
					   (print-key ?p)
					   (decimal-key ?\.)
					   (decimal ".")
					   (numbers nil))
  "By implementing the bulk of home-row-numbers as a macro it can
be compiled away if the user byte-compiles their init and all
arguments are constants."
  (let ((letters (cond
		  ((eql layout 'qwerty) home-row-numbers-qwerty)
		  ((eql layout 'dvorak) home-row-numbers-dvorak)
		  ((eql layout 'qwerty-numpad)
		   (home-row-numbers-numpad-warning numbers)
		   home-row-numbers-qwerty-numpad)
		  ((eql layout 'dvorak-numpad)
		   (home-row-numbers-numpad-warning numbers)
		   home-row-numbers-dvorak-numpad)
		  ((stringp layout)
		   (home-row-numbers-string->char-list layout))
		  (t (cl-assert (consp layout)
			     nil
			     "the LAYOUT argument to
		     home-row-numbers should either be a string
		     or list of characters or one of the symbols
		     specified in the home-row-numbers
		     doc-string")
		     layout)))
	(numbers (cond
		  ((consp numbers) numbers)
		  ((stringp numbers)
		   (home-row-numbers-string->char-list numbers))
		  ((or (eql numbers 'zero)
		       (eql numbers 'zero-first))
		   home-row-numbers-zero)
		  ((or (eql numbers 'programming)
		       (eql numbers 'prog))
		   home-row-numbers-prog)
		  (t home-row-numbers-norm))))
    (cl-assert (= (length letters) (length numbers))
	    nil
	    "the LAYOUT and NUMBERS arguments to home-row-numbers
	    should be the same length")
    `(progn
       (defvar home-row-numbers-already-printed nil
	       "String of what's been printed, for use with
	       decimal functionality")

       (defun home-row-numbers-argument (arg)
	 ,home-row-numbers-argument-doc
	 (interactive "P")
	 (let ((last-command-event
		(cl-case last-command-event
		  ,@(cl-loop for k in letters
			  for n in numbers
			  collect `(,k ,n))
		  (t (user-error
		      "home-row-numbers-argument is not configured for %c"
		      last-command-event)))))
	   (digit-argument arg)
	   ,(when message
	      '(message
		(concat "C-u- "
			home-row-numbers-already-printed
			(number-to-string
			 (prefix-numeric-value prefix-arg)))))
	   prefix-arg))

       ,@(when print-key
	   `((defun home-row-numbers-print (arg)
	       "Insert `prefix-arg' into the current buffer."
	       (interactive "p")
	       (setq home-row-numbers-already-printed nil)
	       (let ((str (number-to-string arg)))
		 (insert str)
		 str))

	     ,@(cl-loop for k in (if (consp print-key)
				     print-key
				   (if (stringp print-key)
				       (home-row-numbers-string->char-list
					print-key)
				     (list print-key)))
		     collect
		     `(define-key universal-argument-map
			[,k] #'home-row-numbers-print))))

       ,@(when decimal-key
	   (cl-assert print-key nil "The DECIMAL-KEY
	   functionality requires at least one PRINT-KEY")
	   `((defun home-row-numbers-decimal (arg)
	       "Insert `prefix-arg' into the current buffer, a
	       decimal, and continue accepting a prefix
	       argument."
	       (interactive "p")
	       (let ((new-part (home-row-numbers-print arg)))
		 (insert ,decimal)
		 (setq home-row-numbers-already-printed
		       (concat home-row-numbers-already-printed
			       new-part
			       ,decimal)))
	       (message (concat "C-u- " home-row-numbers-already-printed))
	       (universal-argument))

	     ,@(cl-loop for k in (if (consp decimal-key)
				     decimal-key
				   (if (stringp decimal-key)
				       (home-row-numbers-string->char-list
					decimal-key)
				     (list decimal-key)))
			collect
			`(define-key universal-argument-map
			   [,k] #'home-row-numbers-decimal))))

       ,@(cl-loop for k in letters
	       collect `(define-key universal-argument-map
			  [,k] #'home-row-numbers-argument)))))

(defun home-row-numbers--completing-read (keyword prompt options required)
  (list keyword
	(intern
	 (completing-read prompt options nil required
			  nil nil (first options)))))

;;;###autoload
(cl-defun home-row-numbers (&key (layout 'qwerty)
				 (message t)
				 (print-key ?p)
				 (decimal-key ?\.)
				 (decimal ".")
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
A string or list of characters can also be provided to be used
instead. LAYOUT keys override PRINT-KEY and DECIMAL-KEY.
Default is qwerty.

MESSAGE

If true the numeric value of `prefix-arg' is printed in the
mini-buffer after each keypress. Default true.

PRINT-KEY

A character to bind `home-row-numbers-print' to. If nil then not
bound. If a string or list of characters all are bound. Default p.

DECIMAL-KEY

A character to bind `home-row-numbers-decimal' to. If nil then
not bound. Requires at least one PRINT-KEY. If a string or list
of characters all are bound. Default period.

DECIMAL

A string to be inserted by `home-row-numbers-decimal'.
Default period.

NUMBERS

One of the symbols: zero-first or programming

The former will move zero to be before one, the latter will
re-order the numbers to be as they are in the programming dvorak
layout. If nil, the default, then order the numbers as they are
on a traditional keyboard layout. Numpad layouts assume this
argument is nil. A string or list of characters can also be
provided to be used instead."
  (interactive
   (progn
     (home-row-numbers-disable)
     (append
      (home-row-numbers--completing-read
       :layout "Layout: "
       '("qwerty" "dvorak" "qwerty-numpad" "dvorak-numpad")
       'confirm)
      (home-row-numbers--completing-read
       :message "Message: "
       '("t" "nil")
       t)
      (list :print-key
	    (home-row-numbers-string->char-list
	     (completing-read "Print-key(s): "
			      '("p")
			      nil nil nil nil
			      "p")))
      (list :decimal-key
	    (home-row-numbers-string->char-list
	     (completing-read "Decimal-key(s): "
			      '("." ",")
			      nil nil nil nil
			      ".")))
      (list :decimal
	    (completing-read "Decimal: "
			     '("." ",")
			     nil nil nil nil
			     "."))
      (home-row-numbers--completing-read
       :numbers "Numbers: "
       '("normal" "zero-first" "programmer")
       'confirm))))
  (eval `(home-row-numbers-helper
	  :layout ,layout
	  :message ,message
	  :print-key ,print-key
	  :decimal-key ,decimal-key
	  :decimal ,decimal
	  :numbers ,(unless (eql numbers 'normal) numbers))
	t)
  (byte-compile #'home-row-numbers-argument)
  (byte-compile #'home-row-numbers-print)
  (byte-compile #'home-row-numbers-decimal))

(define-compiler-macro home-row-numbers (&whole form &rest args)
  (if (cl-every
       (lambda (x)
	 (or (keywordp x)
	     (eq x t)
	     (eq x nil)
	     (integerp x)
	     (stringp x)
	     (and (consp x)
		  (eq (first x)
		      'quote))))
       args)
      `(home-row-numbers-helper
	,@(cl-loop for arg in args collect
		(if (consp arg)
		    (second arg)
		  arg)))
    form))

;;;###autoload
(defun home-row-numbers-disable ()
  "Disable home-row-numbers"
  (interactive)
  (substitute-key-definition 'home-row-numbers-argument
			     nil
			     universal-argument-map)
  (substitute-key-definition 'home-row-numbers-print
			     nil
			     universal-argument-map)
  (substitute-key-definition 'home-row-numbers-decimal
			     nil
			     universal-argument-map))

(provide 'home-row-numbers)
