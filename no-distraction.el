;;;; no-distraction.el --- Don't distract to unimportant things           -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Artur Yaroshenko
;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/no-distraction
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Collection of useful actions to work with buffers.

;;; Code:

(defface no-distraction-face
  '((t :inherit 'font-lock-comment-face))
  "Face for no-distraction elements."
  :group 'no-distraction)

(defmacro no-distraction-get-rule (lang &rest patterns)
  "Generate Tree-sitter font-lock rules by provided PATTERNS for no-distraction LANG."
  `(treesit-font-lock-rules
    ,@(mapcan
       (lambda (p)
         `(:language ,lang
                     :feature 'no-distraction
                     :override t
                     ',p))
       patterns)))

(defcustom no-distraction--ts-rules
  (no-distraction-get-rule 'typescript
                           ((interface_declaration) "interface" @no-distraction-face)
                           ((lexical_declaration) "const")
                           ((accessibility_modifier) "readonly" @no-distraction-face)
                           ((accessibility_modifier) "static" @no-distraction-face)
                           ((import_statement) "import" @no-distraction-face)
                           ((import_statement) "from" @no-distraction-face)
                           ((import_statement) "type" @no-distraction-face)
                           ((export_statement) "export" @no-distraction-face)
                           ((lexical_declaration) "const" @no-distraction-face)
                           ((class_declaration) "class" @no-distraction-face)
                           ((implements_clause) "implements" @no-distraction-face)
                           ((accessibility_modifier) @no-distraction-face)
                           ((member_expression object: (this) @no-distraction-face))
                           (((identifier) @obj (:match "^console$" @obj)) @no-distraction-face)
                           ((if_statement      "if"      @no-distraction-face))
                           ((else_clause       "else"    @no-distraction-face))
                           ((for_statement     "for"     @no-distraction-face))
                           ((while_statement   "while"   @no-distraction-face))
                           ((do_statement      "do"      @no-distraction-face))
                           ((switch_statement  "switch"  @no-distraction-face))
                           ((try_statement     "try"     @no-distraction-face))
                           ((catch_clause      "catch"   @no-distraction-face))
                           ((finally_clause    "finally" @no-distraction-face))
                           ((break_statement   "break"   @no-distraction-face))
                           ((continue_statement "continue" @no-distraction-face))
                           ((throw_statement   "throw"   @no-distraction-face))
                           ((object (pair key: (property_identifier) @default)))
                           ((yield_expression  "yield"   @no-distraction-face))
                           ((await_expression  "await"   @no-distraction-face))
                           ((comment) @font-lock-builtin-face)
                           ((required_parameter (identifier) @default))
                           ((new_expression  "new"   @no-distraction-face))
                           ([ "(" ")" "[" "]" "{" "}" ";" ":" "<" ">" "," "async" ] @no-distraction-face)
                           ((this) @no-distraction-face))
  "Tree-sitter rules for no-distraction in TypeScript."
  :group 'no-distraction)


(defcustom no-distraction--html-rules
  (no-distraction-get-rule 'html
                           ((tag_name) @no-distraction-face)
                           ([ "(" ")" "[" "]" "{" "}" ";" ":" "<" ">" "," ] @no-distraction-face))
  "Tree-sitter rules for no-distraction in HTML."
  :group 'no-distraction)


(defcustom no-distraction--modes-settings
  '((typescript-ts-mode . no-distraction--ts-rules)
    (vue-ts-mode . no-distraction--ts-rules)
    (ng2-ts-mode . no-distraction--ts-rules)
    (html-ts-mode       . no-distraction--html-rules))
  "Association list of major modes and their corresponding treesit rules for no-distraction."
  :group 'no-distraction)


(defun no-distraction--enable-feature (feat)
  "Enable Tree-sitter feature FEAT for current buffer."
  (let ((lvl0 (car treesit-font-lock-feature-list)))
    (unless (memq feat lvl0)
      (setcar treesit-font-lock-feature-list (cons feat lvl0)))))

(defun no-distraction--rules-available-p ()
  "Check if no-distraction rules are available for the current major mode."
  (and (boundp 'treesit-font-lock-settings)
       (treesit-parser-list)
       (assoc-default major-mode no-distraction--modes-settings)))

(defun no-distraction--current-rules ()
  "Get no-distraction Tree-sitter rules for the current major mode."
  (let ((sym (assoc-default major-mode no-distraction--modes-settings)))
    (and (boundp sym) (symbol-value sym))))

(defun no-distraction--feature-enable (feat)
  "Enable Tree-sitter feature FEAT for current buffer."
  (let ((lvl0 (car treesit-font-lock-feature-list)))
    (unless (memq feat lvl0)
      (setcar treesit-font-lock-feature-list (cons feat lvl0)))))

(defun no-distraction--apply-rules ()
  "Apply no-distraction Tree-sitter rules in the current buffer if available."
  (when (no-distraction--rules-available-p)
    (let ((rules (no-distraction--current-rules)))
      (unless (local-variable-p 'no-distraction--saved-treesit-settings)
        (setq-local no-distraction--saved-treesit-settings treesit-font-lock-settings))
      (unless (local-variable-p 'no-distraction--saved-feature-list)
        (setq-local no-distraction--saved-feature-list treesit-font-lock-feature-list))
      (setq-local treesit-font-lock-settings
                  (append treesit-font-lock-settings rules))
      (no-distraction--feature-enable 'no-distraction)
      (treesit-font-lock-recompute-features)
      (font-lock-flush))))


(defun no-distraction--restore-original ()
  "Restore font-lock state saved before enabling no-distraction."
  (when (local-variable-p 'no-distraction--saved-treesit-settings)
    (setq-local treesit-font-lock-settings no-distraction--saved-treesit-settings)
    (kill-local-variable 'no-distraction--saved-treesit-settings))
  (when (local-variable-p 'no-distraction--saved-feature-list)
    (setq-local treesit-font-lock-feature-list no-distraction--saved-feature-list)
    (kill-local-variable 'no-distraction--saved-feature-list))
  (treesit-font-lock-recompute-features)
  (font-lock-flush))

(define-minor-mode no-distraction-mode
  "Toggle Tree-sitter no-distraction font-lock rules in the current buffer.

When enabled, the rules defined in `no-distraction--modes-settings` are applied to
the current buffer if it uses Tree-sitter.  When disabled, the original
`treesit-font-lock-settings` and `treesit-font-lock-feature-list` are
restored."
  :lighter " ND"
  (if no-distraction-mode
      (no-distraction--apply-rules)
    (no-distraction--restore-original)))


(provide 'no-distraction)
;; Local Variables:
;; End:

;;; no-distraction.el ends here
