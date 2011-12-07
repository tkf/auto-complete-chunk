(require 'auto-complete)

(defvar ac-chunk-regex
  "\\(\\s \\|\\s(\\|\\s)\\|\n\\)\\(\\sw\\|\\s_\\)+\\(\\s.\\(\\sw\\|\\s_\\)+\\)*\\="
  )

(defun ac-chunk-beginning ()
  (ignore-errors
    (save-excursion
      (+ (re-search-backward ac-chunk-regex) 1))))

(defun ac-chunk-candidates-from-list (chunk-list)
  "Return matched candidates in CHUNK-LIST"
  (let* ((current (point))
         (start (ac-chunk-beginning))
         (prefix (if start (buffer-substring start current)))
         (prefix-len (if start (length prefix)))
         (chunk-regex (if start (concat "^" prefix))))
    (when start
      (loop for cc in chunk-list
            when (string-match chunk-regex cc)
            collect cc))))

(defvar ac-chunk-list nil)
(make-variable-buffer-local 'ac-chunk-list)
(defun ac-chunk-list () ac-chunk-list)

(defun ac-chunk-list-candidates ()
  "Create candidates from a buffer local variable `ac-chunk-list'"
  (ac-chunk-candidates-from-list ac-chunk-list))

(ac-define-source chunk-list
  '((candidates . ac-chunk-list-candidates)
    (available . ac-chunk-list)
    (requires . 0)
    (prefix . ac-chunk-beginning)
    (symbol . "c")))

(defun ac-dictionary-chunk-candidates ()
  "Create candidates from dictionary (`ac-buffer-dictionary')"
  (ac-chunk-candidates-from-list (ac-buffer-dictionary)))

(ac-define-source dictionary-chunk
  '((candidates . ac-dictionary-chunk-candidates)
    (requires . 0)
    (prefix . ac-chunk-beginning)
    (symbol . "c")))

(defun ac-use-dictionary-chunk ()
  "Swap `ac-source-dictionary' with `ac-source-dictionary-chunk'"
  (setq ac-sources (delq 'ac-source-dictionary ac-sources))
  (add-to-list 'ac-sources 'ac-source-dictionary-chunk)
  )

(defadvice ac-common-setup
  (after ac-common-setup-with-chunk () activate)
  "To make `ac-source-chunk-list' work, it must be at the top of `ac-sources'

Note that `ac-common-setup' is added to `auto-complete-mode-hook'
by `ac-config-default', thus called at the very end of the
`ac-sources' setup.

If this advice is active, `ac-chunk-list' is functional if
`ac-chunk-list' is set."
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
