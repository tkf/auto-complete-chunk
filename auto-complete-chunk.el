;;; auto-complete-chunk.el --- Auto-completion for dot.separated.words.
;;
;; Filename: auto-complete-chunk.el
;; Description: Auto-completion for dot.separated.words.
;; Author: ARAKAKI, Takafumi
;; Maintainer: ARAKAKI, Takafumi
;; Created: Wed Dec 7 17:23:39 2011 +0100
;; URL: https://github.com/tkf/auto-complete-chunk
;;
;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Example usage:
;;    (add-hook
;;     'python-mode
;;     (lambda ()
;;       (setq ac-chunk-list
;;             '("os.path.abspath" "os.path.altsep" "os.path.basename"))))

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'auto-complete)

(defvar ac-chunk-regex
  (rx (group-n 1 (| (syntax whitespace)
                    (syntax open-parenthesis)
                    (syntax close-parenthesis)
                    bol))
      (* (group (+ (| (syntax word) (syntax symbol)))
                (syntax punctuation)))
      (+ (| (syntax word) (syntax symbol)))
      (? (syntax punctuation))
      point)
  "A regexp that matches to a \"chunk\" containing words and dots.")

(defun ac-chunk-beginning ()
  "Return the position where the chunk begins."
  (ignore-errors
    (save-excursion
      (+ (re-search-backward ac-chunk-regex) (length (match-string 1))))))

(defun ac-chunk-candidates-from-list (chunk-list)
  "Return matched candidates in CHUNK-LIST."
  (let* ((current (point))
         (start (ac-chunk-beginning))
         (prefix (if start (buffer-substring start current)))
         (prefix-len (if start (length prefix)))
         (chunk-regex (if start (concat "^" prefix))))
    (when start
      (loop for cc in chunk-list
            when (string-match chunk-regex cc)
            collect cc))))

(defvar ac-chunk-list nil
  "Dictionary used from `ac-source-chunk-list'.  List of strings.")
(make-variable-buffer-local 'ac-chunk-list)

(defun ac-chunk-list ()
  "Util function to access the variable `ac-chunk-list'."
  ac-chunk-list)

(defun ac-chunk-list-candidates ()
  "Create candidates from a buffer local variable `ac-chunk-list'."
  (ac-chunk-candidates-from-list ac-chunk-list))

(ac-define-source chunk-list
  '((candidates . ac-chunk-list-candidates)
    (requires . 0)
    (prefix . ac-chunk-beginning)
    (symbol . "c")))

(defun ac-dictionary-chunk-candidates ()
  "Create candidates from dictionary (variable `ac-buffer-dictionary')."
  (ac-chunk-candidates-from-list (ac-buffer-dictionary)))

(ac-define-source dictionary-chunk
  '((candidates . ac-dictionary-chunk-candidates)
    (requires . 0)
    (prefix . ac-chunk-beginning)
    (symbol . "c")))

(defun ac-use-dictionary-chunk ()
  "Swap `ac-source-dictionary' with `ac-source-dictionary-chunk'."
  (setq ac-sources (delq 'ac-source-dictionary ac-sources))
  (add-to-list 'ac-sources 'ac-source-dictionary-chunk)
  )

(defadvice ac-common-setup
  (after ac-common-setup-with-chunk () activate)
  "To make `ac-source-chunk-list' work, it must be at the top of `ac-sources'.

FIXME: there must be a better way to do it... so, find it out.

Note that `ac-common-setup' is added to `auto-complete-mode-hook'
by `ac-config-default', thus called at the very end of the
`ac-sources' setup.

If this advice is active, `ac-source-chunk-list' is functional when
the variable `ac-chunk-list' is set."
  (setq ac-sources (delq 'ac-source-chunk-list ac-sources))
  (setq ac-sources (delq 'ac-source-filename ac-sources))
  (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
  ;; make yasnippet has lowest priority
  (setq ac-sources
        (append
         '(ac-source-chunk-list ac-source-filename)
         ac-sources
         '(ac-source-yasnippet)
         )))

(provide 'auto-complete-chunk)

;;; auto-complete-chunk.el ends here
