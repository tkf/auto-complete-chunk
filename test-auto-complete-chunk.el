;;; test-auto-complete-chunk.el --- Tests for auto-complete-chunk.el

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; test-auto-complete-chunk.el is free software: you can redistribute
;; it and/or modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.

;; test-auto-complete-chunk.el is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with test-auto-complete-chunk.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)

(require 'auto-complete-chunk)

(defmacro ac-chunk-regex-deftest (name mode insert prefix)
  (declare (indent 1))
  `(ert-deftest ,name ()
     (with-temp-buffer
       (let ((buffer (current-buffer)))
         (,mode)
         (insert ,insert)
         (let ((beg (ac-chunk-beginning)))
           (if ,prefix
               (should (equal (buffer-substring beg (point)) ,prefix))
             (should-not beg)))
         ;; To make sure buffer local variables are removed.
         (kill-buffer buffer)))))

(ac-chunk-regex-deftest ac-chunk-regex/fu/0 fundamental-mode "\na.b" "a.b")
(ac-chunk-regex-deftest ac-chunk-regex/el/0 emacs-lisp-mode  "\na.b" "a.b")
(ac-chunk-regex-deftest ac-chunk-regex/py/0 python-mode      "\na.b" "a.b")
(ac-chunk-regex-deftest ac-chunk-regex/fu/1 fundamental-mode   "a.b" "a.b")
(ac-chunk-regex-deftest ac-chunk-regex/el/1 emacs-lisp-mode    "a.b" "a.b")
(ac-chunk-regex-deftest ac-chunk-regex/py/1 python-mode        "a.b" "a.b")
(ac-chunk-regex-deftest ac-chunk-regex/fu/2 fundamental-mode   "a."  "a.")
(ac-chunk-regex-deftest ac-chunk-regex/el/2 emacs-lisp-mode    "a."  "a.")
(ac-chunk-regex-deftest ac-chunk-regex/py/2 python-mode        "a."  "a.")
(ac-chunk-regex-deftest ac-chunk-regex/fu/3 fundamental-mode   "a"   "a")
(ac-chunk-regex-deftest ac-chunk-regex/el/3 emacs-lisp-mode    "a"   "a")
(ac-chunk-regex-deftest ac-chunk-regex/py/3 python-mode        "a"   "a")

;; (ert "ac-chunk-regex/fu/.*")


(provide 'test-auto-complete-chunk)

;;; test-auto-complete-chunk.el ends here
