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
;; Helper functions for NodeJS interaction. Works in js2-mode.
;;

(require 'js-comint)
;;(setenv "NODE_NO_READLINE" "1")
;;(setq inferior-js-program-command "node --interactive")
(setq inferior-js-program-command "node")

;; (fset 'screen-width #'frame-width)

(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))))

(defun node-js-local-mode-map ()
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go)
  )

(defun node-js-eval-region-or-buffer ()
  "Evaluate the current buffer (or region if mark-active),
   and return the result into another buffer,
   which is to be shown in a window."
  (interactive)
  (let ((debug-on-error t) (start 1) (end 1))
    (cond
     (mark-active
      (setq start (point))
      (setq end (mark)))
     (t
      (setq start (point-min))
      (setq end (point-max))))
    (call-process-region
     start end     ; seems the order does not matter
     "node"        ; node.js
     nil           ; don't delete region
     "*js*"        ; output buffer
     nil)          ; no redisply during output
    (message "Region or buffer evaluated!")
    (setq deactivate-mark t))) ; deactive the region, regardless

(define-key global-map (kbd "C-c v") 'node-js-eval-region-or-buffer)


