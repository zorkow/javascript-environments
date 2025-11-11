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
      (append  '(("\\.ts$" . typescript-mode)
                 ("\\.tsx$" . typescript-mode))
               auto-mode-alist))

(load-library "cl-lib")
(load-library "jsdoc-helpers")
(load-library "nodejs-helpers")
;; (load-library "jsdoc-highlighter")
;;
;; Some TypeScript extensions
;;


(add-hook 'typescript-mode-hook
          #'(lambda ()
              ;;(setq font-lock-comment-face 'font-lock-string-face)
              (setq typescript-indent-level 2)
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              ;; (setq flycheck-typescript-tslint-executable (tslint-binary))
              (electric-pair-mode)
              (auto-complete-mode)
              (eldoc-mode +1)
              ;; (tss-setup-current-buffer)
              (jsdoc-local-mode-map)
              (typescript-local-mode-map)
              (node-js-local-mode-map)
              ))

(add-hook 'typescript-mode-local-vars-hook
          (lambda ()
            (eslint-set-executable)
            (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)))

(defconst typescript-local-executable "tsc")
(defconst tslint-executable "tslint")
(defconst tslint-formatter "tslint-unix-formatter")
(defconst tslint-rules "tslint-jsdoc-rules")


(defun typescript-local-nodemodules ()
  (let ((dir (locate-dominating-file (buffer-file-name) "node_modules")))
    (when dir (concat dir "node_modules/"))))

(defvar tss-local-path-extended nil)
(defun tss-local-extend-path ()
  (print "HERE")
  (when (not tss-local-path-extended)
    (let ((nodemodules (typescript-local-nodemodules)))
      (print nodemodules)
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

(defun eslint-buffer ()
  (interactive)
  (let* ((dir (locate-dominating-file (buffer-file-name) "tsconfig.json"))
         (file (file-relative-name (buffer-file-name) dir)))
    (compile (format "cd %s; %s --format unix %s" dir (eslint-binary) file) nil)
  ))

(defun eslint-package ()
  (interactive)
  (let* ((dir (locate-dominating-file (buffer-file-name) "tsconfig.json"))
         (file (file-relative-name (buffer-file-name) dir))
         (subdir (subseq file 0 (search "/" file)))
         )
    (compile (format "cd %s; %s --format unix %s" dir (eslint-binary) subdir))
    ))

(defun eslint-module ()
  (interactive)
  (let* ((dir (locate-dominating-file (buffer-file-name) "tsconfig.json"))
         (file (file-relative-name (buffer-file-name) dir))
         (subdir (subseq file 0 (search "/" file :from-end t)))
         )
    (compile (format "cd %s; %s --format unix %s" dir (eslint-binary) subdir))
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

(defun typescript-local-function-comment ()
  (interactive)
  (let ((regexp "\\(public\\)\\|\\(private\\)\\|\\(protected\\)\\|\\(function\\)"))
    (search-forward-regexp regexp)
    (search-forward "(")
    (let* ((start (point))
           (stop (progn
                   (backward-char 1)
                   (forward-sexp)
                   (point)))
           (selection (buffer-substring-no-properties start (- stop 1)))
           (params (split-string selection ","))
           (pairs (do* ((params params (cdr params))
                        param)
                      ((null params) param)
                    (push (split-string (car params) ":") param)
                    ))
           (count 0)
           (brace (progn
                    (search-forward "{")
                    (point)))
           (type (string-trim (buffer-substring-no-properties stop (- brace 1))))
           (result (when (> (length type) 0)
                     (string-trim (subseq type 1))))
           )
    (search-backward-regexp regexp)
    (forward-char 1)
    (beginning-of-line)
    (insert "\n/**\n * \n")
    (do* ((pairs (reverse pairs) (cdr pairs))
          (pair (car pairs) (car pairs)))
        ((null pairs))
      (when (cadr pair)
        (incf count)
        (insert (concat " * @param {" (string-trim (mapconcat 'identity (cdr pair) ":")) "} "
                        (string-trim (car pair)) " \n"))))
    (when (> (length result) 0)
        (incf count)
        (insert (concat " * @returns {" result "} \n")))
    (insert " */\n")
    (jsdoc-mode-indent-region (+ count 3) 1 0))
  ))


(defun typescript-local-type-comment ()
  (interactive)
  (let ((regexp "\\(public\\)\\|\\(private\\)\\|\\(protected\\)\\|\\(let\\)"))
    (search-forward-regexp regexp)
    (search-forward ":")
    (let* ((start (point))
           (stop (progn
                   (backward-char 1)
                   (search-forward-regexp "\\(;\\|=\\)")
                   (point)))
           (type (string-trim (buffer-substring-no-properties start (- stop 1)))))
    (search-backward-regexp regexp)
    (forward-char 1)
    (beginning-of-line)
    (insert "\n/**\n * \n")
    (insert (concat " * @type {" (string-trim type) "}\n")))
    (insert " */\n")
    (jsdoc-mode-indent-region 4 1 0))
  )


(defun typescript-local-mode-map ()
  (local-set-key "\C-c\C-c" 'typescript-local-compile)
  ;;(local-set-key "\C-c\C-v" 'typescript-local-compile-go-node)
  (local-set-key "\C-c\C-v" 'typescript-local-go-node)
  (local-set-key "\C-cf" 'typescript-local-function-comment)
  (local-set-key "\C-ct" 'typescript-local-type-comment)
  )


(defun eslint-binary ()
  (let ((nodemodules (typescript-local-nodemodules)))
    (if nodemodules
        (concat nodemodules ".bin/eslint")
      "eslint")))

(defun eslint-set-executable ()
  (setq-local flycheck-javascript-eslint-executable (esline-binary))
  )
