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
;; Helper functions for TypeScript.
;;


(autoload 'typescript-mode "typescript-mode")
(setq auto-mode-alist
   (cons  '("\\.ts$" . typescript-mode)
          auto-mode-alist))

(load-library "jsdoc-helpers")
(load-library "nodejs-helpers")
(load-library "jsdoc-highlighter")
;;
;; Some TypeScript extensions
;;



(add-hook 'typescript-mode-hook
          #'(lambda ()
              ;;(setq font-lock-comment-face 'font-lock-string-face)
              (setq typescript-indent-level 2)
              (tide-setup)
              (flymake-mode)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              ;; setup the linter.
              (flycheck-typescript-tslint-setup)
              (eldoc-mode +1)
              (tss-setup-current-buffer)
              (jsdoc-local-mode-map)
              (typescript-local-mode-map)
              (node-js-local-mode-map)
              ))


(defun tslint-buffer ()
  (interactive)
  (compile (format "tslint -t unix -s ~/node_modules/tslint-unix-formatter/lib/ %s" (buffer-file-name)) nil)
  )

(defun tslint-package ()
  (interactive)
  (let ((src-directory (file-name-directory (buffer-file-name))))
    (compile (format "tslint -t unix -s ~/node_modules/tslint-unix-formatter/lib/ %s/*.ts" src-directory) nil)
    ))

(defun typescript-compile ()
  (interactive)
  (let ((dir (locate-dominating-file (buffer-file-name) "tsconfig.json")))
    (if dir 
        (compile (format "cd %s; tsc | sed -e 's/\(/:/' -e 's/,/:/' -e 's/\)//'; cd -" dir) nil)
      (compile (format "tsc | sed -e 's/\(/:/' -e 's/,/:/' -e 's/\)//'") nil)
      )))

(defun typescript-compile-go-node ()
  (interactive)
  (typescript-compile)
  (typescript-go-node)
  )

;TODO: This really should also automatically load the js file.
(defun typescript-go-node ()
  (interactive)
  (let* ((dir (file-truename
               (locate-dominating-file (buffer-file-name) "tsconfig.json")))
         (file (find-file (concat dir "tsconfig.json")))
         (json (jsons-parse))
         (out (typescript-get-out-file json)))
    (kill-buffer)
    (when out
      (comint-send-string inferior-js-buffer
                          (concat ".load " (file-truename dir) out "\n")))
    (run-js inferior-js-program-command nil)
    ))


(defun typescript-get-out-file (json)
  (when json
    (let ((compilerOptions (gethash "\"compilerOptions\"" (cadr json))))
      (when compilerOptions
        (let ((out (gethash "\"out\"" (cadr compilerOptions))))
          (when out
            (let ((str (cadr out)))
              (subseq str 1 (1- (length str))))))))))


(defun jslint-typescript-buffer ()
  (interactive)
  (compile (format "gjslint --unix_mode --strict --jsdoc %s" (buffer-file-name)) nil)
  )

(defun typescript-local-mode-map ()
  (local-set-key "\C-c\C-c" 'typescript-compile)
  ;;(local-set-key "\C-c\C-v" 'typescript-compile-go-node)
  (local-set-key "\C-c\C-v" 'typescript-go-node)
  )

