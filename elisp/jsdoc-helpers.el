;; Copyright (C) 2015, 2016 The MathJax Consortium

;; Author: Volker Sorge <v.sorge@mathjax.org>

;;  Licensed under the Apache License, Version 2.0 (the "License");
;;  you may not use this file except in compliance with the License.
;;  You may obtain a copy of the License at
;; 
;;      http://www.apache.org/licenses/LICENSE-2.0
;; 
;;  Unless required by applicable law or agreed to in writing, software
;;  distributed under the License is distributed on an "AS IS" BASIS,
;;  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;  See the License for the specific language governing permissions and
;;  limitations under the License.

;; 
;; Helper functions for JSDoc. Works in js2-mode.
;;


(defun jsdoc-mode-indent-region (height down back)
  (let ((end (cadr (posn-at-point))))
    (beginning-of-line)
    (previous-line height)
    (indent-region (cadr (posn-at-point)) end)
    (next-line down)
    (end-of-line)
    (backward-char back)
    )
  )


(defun jsdoc-mode-multi-comment ()
  (interactive)
  (insert "\n/**\n *  \n *\n *\n */\n")
  (jsdoc-mode-indent-region 5 1 0)
)


(defun jsdoc-mode-function-comment ()
  (interactive)
  (insert "\n/**\n * \n * @param {}\n * @return {}\n */\n")
  (jsdoc-mode-indent-region 5 1 0))


(defun jsdoc-mode-constructor-comment ()
  (interactive)
  (insert "\n/**\n * @constructor\n * @extends {}\n */\n")
  (jsdoc-mode-indent-region 4 2 1)
)


(defun jsdoc-mode-override-comment ()
  (interactive)
  (insert "\n/**\n * @override\n */\n")
  (jsdoc-mode-indent-region 3 3 0)
  (indent-for-tab-command)
)

(defun jsdoc-mode-type-comment ()
  (interactive)
  (insert "\n/**\n * @type {}\n */\n")
  (jsdoc-mode-indent-region 3 1 1)
)

(defun jsdoc-mode-add-parameter-comment ()
  (interactive)
  (end-of-line)
  (insert "\n * @param {}")
  (indent-for-tab-command)
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
  (search-backward-regexp "[ |<]")
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
  (local-set-key "\C-ce" 'jsdoc-mode-multi-comment)
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

