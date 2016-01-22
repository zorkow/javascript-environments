;; Copyright (C) 2015, 2016 Volker Sorge <volker.sorge@gmail.com>

;; Author: Volker Sorge <volker.sorge@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; 
;; Helper functions for JSDoc. Works in js2-mode.
;;

(defun jsdoc-mode-multi-comment ()
  (interactive)
  (insert "\n/**\n *  \n *\n *\n */\n")
  (previous-line 4)
  (forward-char 3)
)


(defun jsdoc-mode-function-comment ()
  (interactive)
  (insert "\n/**\n * \n * @param {}\n * @return {}\n */\n")
  (previous-line 4)
  (forward-char 3)
)


(defun jsdoc-mode-constructor-comment ()
  (interactive)
  (insert "\n/**\n * @constructor\n * @extends {}\n */\n")
  (previous-line 2)
  (forward-char 13)
)


(defun jsdoc-mode-override-comment ()
  (interactive)
  (insert "\n/**\n * @override\n */\n")
)

(defun jsdoc-mode-type-comment ()
  (interactive)
  (insert "\n/**\n * @type {}\n */\n")
  (previous-line 1)
  (indent-for-tab-command)
  (previous-line 1)
  (indent-for-tab-command)
  (previous-line 1)
  (indent-for-tab-command)
)

(defun jsdoc-mode-add-parameter-comment ()
  (interactive)
  (end-of-line)
  (insert "\n * @param {}")
  (backward-char 1)
  )

(defun jsdoc-mode-add-console-log ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert "console.log();")
  (indent-for-tab-command)
  (backward-char 2)
  )

(defun jsdoc-mode-rewrite-string-line-break ()
  (interactive)
  (beginning-of-line)
  (search-forward "+")
  (backward-char 1)
  (set-mark (point))
  (search-forward "'")
  (backward-char 1)
  (delete-region (point) (mark))
  (previous-line 1)
  (end-of-line)
  (insert " +")
  (forward-char 1)
)

(defun jsdoc-mode-make-80-character-lines ()
  (interactive)
  (jsdoc-goto-long-line 80)
  (forward-char 76)
  (search-backward " ")
  (js2-line-break)
  (jsdoc-mode-rewrite-string-line-break)
  (previous-line 1)
  (beginning-of-line)
)

;; From http://www.emacswiki.org/emacs/misc-cmds.el
(defun jsdoc-goto-long-line (len)
  "Go to the first line that is at least LEN characters long.
Use a prefix arg to provide LEN.
Plain `C-u' (no number) uses `fill-column' as LEN."
  (interactive "P")
  (setq len  (if (consp len) fill-column (prefix-numeric-value len)))
  (let ((start-line                 (line-number-at-pos))
        (len-found                  0)
        (found                      nil)
        (inhibit-field-text-motion  t))
    (while (and (not found) (not (eobp)))
      (forward-line 1)
      (setq found  (< len (setq len-found  (- (line-end-position) (point))))))
    (if found
        (when (interactive-p)
          (message "Line %d: %d chars" (line-number-at-pos) len-found))
      (goto-line start-line)
      (message "Not found"))))


;; Already existing functions
(defun jsdoc-local-mode-map ()
  (local-set-key "\C-c;" 'comment-region)
  (local-set-key "\C-c:" 'uncomment-region)

  (local-set-key "\C-cr" 'jsdoc-mode-rewrite-string-line-break)
  (local-set-key "\C-c\C-c" 'jsdoc-mode-multi-comment)
  (local-set-key "\C-cf" 'jsdoc-mode-function-comment)
  (local-set-key "\C-cc" 'jsdoc-mode-constructor-comment)
  (local-set-key "\C-co" 'jsdoc-mode-override-comment)
  (local-set-key "\C-ct" 'jsdoc-mode-type-comment)
  (local-set-key "\M-\r" 'jsdoc-mode-add-parameter-comment)
  (local-set-key [C-return] 'js2-line-break)
  (local-set-key "\C-c\C-d" 'jsdoc-mode-add-console-log)
  (local-set-key "\C-cj" 'jsdoc-mode-make-80-character-lines)
  ;;(local-set-key [return] 'js2-line-break)
  ;;(local-set-key "\M-k" 'kill-sexp)
  ;;(local-set-key "\C-\M-k" 'save-kill-sexp)
  )

