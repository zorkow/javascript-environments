# Enforce JSDoc comments via tslint

A rule set for [tslint](https://github.com/palantir/tslint) that enfoces JSDoc
comments on typescript code.

Making sure that all code elements are well documented improves code
readability. It also helps with JSDoc generation as well as transpilation to
compilers like Google's closure.

    "jsdoc-require": [
      true
    ],

Other arguments may be optionally provided:

* \`"no-methods"\` excludes JSDoc comments on interface specifications and class methods
* \`"no-constructors"\` excludes JSDoc comments on class constructors
* \`"no-properties"\` excludes JSDoc comments on class properties
* \`"no-functions"\` excludes JSDoc comments on all functions
* \`"no-protected"\` excludes JSDoc comments on protected elements
* \`"no-private"\` excludes JSDoc comments on private elements
* \`"no-private-properties"\` excludes private properties from enforcing JSDoc comments.


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
