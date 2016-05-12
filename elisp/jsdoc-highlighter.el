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
;; Helper functions for JSDoc in typescript mode.
;;

(defface typescript-jsdoc-tag
  '((t :foreground "SlateGray"))
  "Face used to highlight @whatever tags in jsdoc comments."
  :group 'font-lock-faces)
(defvar typescript-jsdoc-tag 'typescript-jsdoc-tag)

(defface typescript-jsdoc-type
  '((t :foreground "SteelBlue"))
  "Face used to highlight {FooBar} types in jsdoc comments."
  :group 'typescript-mode)
(defvar typescript-jsdoc-type 'typescript-jsdoc-type)

(defface typescript-jsdoc-value
  '((t :foreground "PeachPuff3"))
  "Face used to highlight tag values in jsdoc comments."
  :group 'typescript-mode)
(defvar typescript-jsdoc-value 'typescript-jsdoc-value)

(defface typescript-jsdoc-html-tag-name
  '((((class color) (min-colors 88) (background light))
     (:foreground "rosybrown"))
    (((class color) (min-colors 8) (background dark))
     (:foreground "yellow"))
    (((class color) (min-colors 8) (background light))
     (:foreground "magenta")))
    "Face used to highlight jsdoc html tag names"
  :group 'typescript-mode)
(defvar typescript-jsdoc-html-tag-name 'typescript-jsdoc-html-tag-name)

(defface typescript-jsdoc-html-tag-delimiter
  '((((class color) (min-colors 88) (background light))
     (:foreground "dark khaki"))
    (((class color) (min-colors 8) (background dark))
     (:foreground "green"))
    (((class color) (min-colors 8) (background light))
     (:foreground "green")))
  "Face used to highlight brackets in jsdoc html tags."
  :group 'typescript-mode)
(defvar typescript-jsdoc-html-tag-delimiter 'typescript-jsdoc-html-tag-delimiter)

(defface typescript-simple-comment-face
  '((t :foreground "Firebrick"))
  "Face used to highlight // type comments."
  :group 'typescript-mode)



(defconst typescript-jsdoc-param-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@"
          "\\(?:param\\|argument\\)"
          "\\)"
          "\\s-*{\\([^}]+\\)}?"         ; optional type
          "\\s-*\\[?\\([[:alnum:]_$\.]+\\)?\\]?"  ; name
          "\\>")
  "Matches jsdoc tags with optional type and optional param name.")

(defconst typescript-jsdoc-typed-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("enum"
             "extends"
             "field"
             "id"
             "implements"
             "lends"
             "mods"
             "requires"
             "return"
             "returns"
             "type"
             "typedef"
             "throw"
             "throws"))
          "\\)\\)\\s-*{\\([^}]+\\)}?")
  "Matches jsdoc tags with optional type.")

(defconst typescript-jsdoc-arg-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("alias"
             "augments"
             "borrows"
             "bug"
             "base"
             "config"
             "default"
             "define"
             "exception"
             "function"
             "member"
             "memberOf"
             "name"
             "namespace"
             "property"
             "see"
             "since"
             "suppress"
             "this"
             "throws"
             "version"))
          "\\)\\)\\s-+\\([^ \t]+\\)")
  "Matches jsdoc tags with a single argument.")

(defconst typescript-jsdoc-empty-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("addon"
             "author"
             "class"
             "const"
             "constant"
             "constructor"
             "constructs"
             "deprecated"
             "desc"
             "description"
             "event"
             "example"
             "exec"
             "export"
             "fileoverview"
             "final"
             "function"
             "hidden"
             "ignore"
             "implicitCast"
             "inheritDoc"
             "inner"
             "interface"
             "license"
             "noalias"
             "noshadow"
             "notypecheck"
             "override"
             "owner"
             "preserve"
             "preserveTry"
             "private"
             "protected"
             "public"
             "static"
             "supported"
             ))
          "\\)\\)\\s-*")
  "Matches empty jsdoc tags.")

(defconst typescript-jsdoc-link-tag-regexp
  "{\\(@\\(?:link\\|code\\)\\)\\s-+\\([^#}\n]+\\)\\(#.+\\)?}"
  "Matches a jsdoc link or code tag.")

(defconst typescript-jsdoc-html-tag-regexp
  "\\(</?\\)\\([[:alpha:]]+\\)\\s-*\\(/?>\\)"
  "Matches a simple (no attributes) html start- or end-tag.")

(font-lock-add-keywords 'typescript-mode
                        (list
                         '("\\(//.*\\)" 1 'typescript-simple-comment-face t)
                         (cons typescript-jsdoc-param-tag-regexp
                               '((1 'typescript-jsdoc-tag t)
                                 (2 'typescript-jsdoc-type t)
                                 (3 'typescript-jsdoc-value t)))
                         (cons typescript-jsdoc-typed-tag-regexp
                               '((1 'typescript-jsdoc-tag t)
                                 (2 'typescript-jsdoc-type t)))
                         (cons typescript-jsdoc-arg-tag-regexp
                               '((1 'typescript-jsdoc-tag t)
                                 (2 'typescript-jsdoc-value t)))
                         (cons typescript-jsdoc-empty-tag-regexp
                               '((1 'typescript-jsdoc-tag t)))
                         (cons typescript-jsdoc-link-tag-regexp
                               '((1 'typescript-jsdoc-tag t)
                                 (2 'typescript-jsdoc-html-tag-name t)))
                         (cons typescript-jsdoc-html-tag-regexp
                               '((1 'typescript-jsdoc-html-tag-delimiter t)
                                 (2 'typescript-jsdoc-html-tag-name t)
                                 (3 'typescript-jsdoc-html-tag-delimiter t)))
                         ))
