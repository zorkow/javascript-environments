# UNIX Style Formatter for tslint Output

A simple formatter that produces error output in Unix style. This is similar to
the --unix_mode option of gjslint. The mode is particularly useful for running
inside Emacs.

Install as a node module using npm:

     npm install tslint-jsdoc-rules


To use inside Emacs include the following code in your .emacs file.

    (defun tslint-buffer ()
      (interactive)
      (compile (format "tslint -r ~/node_modules/tslint-jsdoc-rules/lib/ %s" (buffer-file-name)) nil)
      )
      
Works with

    "typescript": ">=1.7.5"
    "tslint": ">=3.2.2"
