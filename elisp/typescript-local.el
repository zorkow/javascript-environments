
(autoload 'typescript-mode "typescript-mode")
(setq auto-mode-alist
   (cons  '("\\.ts$" . typescript-mode)
          auto-mode-alist))

(load-library "jsdoc-helpers")
(load-library "nodejs-helpers")
;;
;; Some TypeScript extensions
;;

(add-hook 'typescript-mode-hook
          #'(lambda ()
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
              (node-js-local-mode-map)
              ))


(defun tslint-buffer ()
  (interactive)
  (compile (format "tslint -t unix -s ~/node_modules/tslint-unix-formatter/lib/ %s" (buffer-file-name)) nil)
  )

(defun typescript-compile ()
  (interactive)
  (compile (format "tsc | sed -e 's/\(/:/' -e 's/,/:/' -e 's/\)//'") nil)
  )

