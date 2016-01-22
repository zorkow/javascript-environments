
(autoload 'js2-mode "js2-mode" nil t)
;;(load-library "js2-mode")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(load-library "jsdoc-helpers")
(load-library "nodejs-helpers")
;;
;; Some Java Script extensions
;;

(add-hook 'js2-mode-hook '(lambda ()
                            (setq js2-basic-offset 2)
                            (setq column-number-mode t)
                            (setq autopair-dont-activate t)
			    (jsdoc-local-mode-map)
                            (node-js-local-mode-map)
			    (electric-pair-mode)
                            )
          )

(defun jslint-buffer ()
  (interactive)
  (compile (format "gjslint --unix_mode --strict --jsdoc %s" (buffer-file-name)) nil)
  )


(load-library "closure-glslint")


