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

(defconst typescript-local-executable "tsc")
(defconst tslint-executable "tslint")
(defconst tslint-formatter "tslint-unix-formatter")
(defconst tslint-rules "tslint-jsdoc-rules")


(defun typescript-local-nodemodules ()
  (let ((dir (locate-dominating-file (buffer-file-name) "node_modules")))
    (when dir (concat dir "node_modules/"))))

(defvar tss-local-path-extended nil)
(defun tss-local-extend-path ()
  (when (not tss-local-path-extended)
    (let ((nodemodules (typescript-local-nodemodules)))
      (when (and nodemodules (file-exists-p (concat nodemodules "typescript-tools/bin/tss")))
        (push (concat nodemodules "typescript-tools/bin/") exec-path)
        (setq tss-local-path-extended t)))))
      
(defun tslint-binary ()
  (let ((nodemodules (typescript-local-nodemodules)))
    (if nodemodules
        (concat nodemodules "tslint/bin/" tslint-executable)
      tslint-executable)))

(defun tslint-options ()
  (let ((nodemodules (typescript-local-nodemodules)))
    (if nodemodules
        (concat " -t unix"
          " -s " nodemodules tslint-formatter "/lib"
          " -r "  nodemodules tslint-rules "/lib")
      (concat " -t unix"
              " -s ~/node_modules/tslint-unix-formatter/lib"
              " -r ~/node_modules/tslint-jsdoc-rules/lib"))))

(defun typescript-local-compiler ()
  (let ((nodemodules (typescript-local-nodemodules)))
    (if nodemodules
        (concat nodemodules "typescript/bin/" typescript-local-executable)
      typescript-local-executable)))

(defun tslint-buffer ()
  (interactive)
  (compile (format "%s %s %s" (tslint-binary) (tslint-options) (buffer-file-name)) nil)
  )

(defun tslint-package ()
  (interactive)
  (let ((src-directory (file-name-directory (buffer-file-name))))
    (compile (format "%s %s %s/*.ts" (tslint-binary) (tslint-options) src-directory) nil)
    ))

(defun typescript-local-compile ()
  (interactive)
  (let* ((dir (locate-dominating-file (buffer-file-name) "tsconfig.json"))
         (tsc (typescript-local-compiler)))
    (if dir 
        (compile (format "cd %s; %s | sed -e 's/\(/:/' -e 's/,/:/' -e 's/\)//'; cd -" dir tsc) nil)
      (compile (format "%s | sed -e 's/\(/:/' -e 's/,/:/' -e 's/\)//'" tsc) nil)
      )))

(defun typescript-local-compile-go-node ()
  (interactive)
  (typescript-local-compile)
  (typescript-local-go-node)
  )

;TODO: This really should also automatically load the js file.
(defun typescript-local-go-node ()
  (interactive)
  (let* ((dir (file-truename
               (locate-dominating-file (buffer-file-name) "tsconfig.json")))
         (file (find-file (concat dir "tsconfig.json")))
         (json (jsons-parse))
         (out (typescript-local-get-out-file json)))
    (kill-buffer)
    (when out
      (comint-send-string inferior-js-buffer
                          (concat ".load " (file-truename dir) out "\n")))
    (run-js inferior-js-program-command nil)
    ))


(defun typescript-local-get-out-file (json)
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
  (local-set-key "\C-c\C-c" 'typescript-local-compile)
  ;;(local-set-key "\C-c\C-v" 'typescript-local-compile-go-node)
  (local-set-key "\C-c\C-v" 'typescript-local-go-node)
  )
