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
  (let ((letters (cond
		  ((eql layout 'qwerty) home-row-numbers-qwerty)
		  ((eql layout 'dvorak) home-row-numbers-dvorak)
		  ((eql layout 'qwerty-numpad)
		   (home-row-numbers-numpad-warning numbers)
		   home-row-numbers-qwerty-numpad)
		  ((eql layout 'dvorak-numpad)
		   (home-row-numbers-numpad-warning numbers)
		   home-row-numbers-dvorak-numpad)
		  (t (assert (= (length layout) 10))
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
