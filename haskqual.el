;;; init.el --- Haskqual -*- lexical-binding: t -*-

;; Copyright (c) 2019 Daniel Matichuk

;; Author: Daniel Matichuk <danmatichuk@gmail.com>
;; Maintainer: Daniel Matichuk <danmatichuk@gmail.com>
;; Created: December 2019
;; Keywords: haskell, tools
;; Version: 0-pre

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Haskqual: Qualified haskell cross-reference lookup

;; This attempts to look up Haskell identifiers by first expanding them
;; to be fully-qualified. This is done by interpreting the import header
;; in order to expand re-qualified modules and limit the
;; search to modules that are in scope.

;; This assumes that the current tags file contains fully-qualified
;; identifiers (e.g. running fast-tags in --fully-qualified mode).

;; Code.

(defun haskqual-parse-module-name (buffer)
  "Parse a haskell buffer for the module name"
  (save-excursion
    (with-current-buffer buffer
      (beginning-of-buffer)
      (re-search-forward
       "^\\s-*module\\s-*\\([A-Za-z0-9.]*\\)" nil :no-error)
      (match-string-no-properties 1))))

(defclass haskqual-import ()
  ((name :type string :initarg :name
         :reader haskqual-import-name )
   (requalified :type boolean :initarg :requalified
                :reader haskqual-import-requalified
                )   
   (requalifier :type string :initarg :requalifier
                :reader haskqual-import-requalifier
                )
   (qualified :type boolean :initarg :qualified
              :reader haskqual-import-qualified))
  :comment "Describes a single haskell module import.")

(defun haskqual-make-import (name requalifier qualified)
  (let* ((requalified (if requalifier t nil))
         (requalifier-string (if requalified requalifier ""))
         (qualified-bool (if qualified t nil)))
                             
  (make-instance 'haskqual-import :name name :requalified requalified :requalifier requalifier-string :qualified qualified-bool)))

(defun haskqual-parse-imports (buffer)
  "Parse a haskell buffer for module imports."
  (save-excursion
    (with-current-buffer buffer
      (beginning-of-buffer)
      (let ((dict nil)
            (sep "\\(?:\\s-\\|\n\\)*\\(?:--.*\n\\)?\\(?:\\s-\\|\n\\)*")
            )
        (while (re-search-forward
                (concat "^import"
                        sep
                        "\\(qualified\\)?"
                        sep
                        "\\([A-Za-z0-9.]*\\)"
                        sep
                        (concat "\\(?:" "as" sep "\\([A-Za-z0-9.]*\\)" "\\)?")
                        )
                nil :no-error)
          (push (haskqual-make-import (match-string-no-properties 2) (match-string-no-properties 3) (match-string-no-properties 1)) dict))
        dict))))

(defun haskqual-parse-exports (buffer)
  "Parse a haskell buffer for module exports"
  (save-excursion
    (with-current-buffer buffer
      (beginning-of-buffer)
      (let ((dict nil)
            (sep "\\(?:\\s-\\|\n\\)*\\(?:--.*\n\\)?\\(?:\\s-\\|\n\\)*")
            )
        (while (re-search-forward
                (concat "[,(]"
                        sep
                        "module"
                        sep
                        "\\([A-Za-z0-9.]*\\)")
                nil :no-error)
          (push (match-string-no-properties 1) dict))
        dict))))

(defun haskqual-expand-qualifier (qualname buffer)
  "Expand local qualifiers and module exports for a qualified identifier, producing a list of possible expansions."
  (let ((quals))
    (dolist (elt (haskqual-parse-imports buffer) quals)
      (when (or (and qualname (string= qualname (haskqual-import-name elt))) ;; is a fully qualified module
             (or
               (and (not qualname) (not (haskqual-import-qualified elt))) ;; unqualified identifier can match any unqualified import
               (and qualname (string= qualname (haskqual-import-requalifier elt))))) ;; qualifier matches "as"
        (push (haskqual-import-name elt) quals)))))
        

(defun haskqual-get-xref (ident buffer)
  "Search for an identifer by first expanding it into possible fully-qualified variants"
  (when ident
    (let* ((match (string-match "^\\(\\(?:[A-Za-z0-9.]+\\)+\\)[.]\\([A-Za-z0-9]+\\)" ident))
         (basename (if match (match-string 2 ident) ident))
         (qualname (if match (match-string 1 ident) nil))
         (qualifiers (haskqual-expand-qualifier qualname buffer))
         (qualified-idents (--map (concat it "." basename) qualifiers))
         (modulename (haskqual-parse-module-name buffer))
         (allqualifiers (if qualname qualified-idents (cons (concat modulename "." ident) qualified-idents)))
         (xref-result (xref--get-first-xref allqualifiers))
         )
    (if xref-result xref-result
      (let ((exports))
        (dolist (elt qualifiers exports)
          (let* ((module-xref (nth 0 (xref-get-xrefs '(etags--xref-backend) elt)))
                 (module-exports (if module-xref (haskqual-parse-exports (xref--buffer-of module-xref)) nil)))
            (setq exports (append module-exports exports))))
        (xref--get-first-xref (--map (concat it "." basename) exports)))))))

(defun haskqual-find-definition ()
  (interactive)
  (let ((xrefs (haskqual-get-xref (haskqual-ident-at-point) (current-buffer))))
    (if xrefs (xref--show-xrefs xrefs nil) (message "No definitions found."))))

(defun haskqual-find-definition-other-window ()
  (interactive)
  (let ((xrefs (haskqual-get-xref (haskqual-ident-at-point) (current-buffer))))
    (if xrefs (xref--show-xrefs xrefs 'window) (message "No definitions found."))))


(defun xref--buffer-of (item)
  (let* ((marker (save-excursion
                   (xref-location-marker (xref-item-location item))))
         (buf (marker-buffer marker)))
    buf))

(defun xref--get-first-xref (inputs)
  (if (null inputs) nil
    (let* ((input (car inputs))
           (xrefs (xref--get-definitions '(etags--xref-backend) input)))
      (if xrefs xrefs
         (xref--get-first-xref (cdr inputs))))))

(defun xref--get-definitions (backend input)
  (let* ((xref-backend-functions backend)
         (xrefs (funcall 'xref-backend-definitions
                         (xref-find-backend)
                         input)))
    xrefs))

;; Copied from dante
(defun haskqual-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (let ((reg (dante-ident-pos-at-point)))
    (when reg
      (apply #'buffer-substring-no-properties reg))))

;; Copied from dante
(defun haskqual-ident-pos-at-point (&optional offset)
  "Return the span of the (qualified) identifier at point+OFFSET, or nil if none found."
  (let* ((qualifier-regex "\\([[:upper:]][[:alnum:]]*\\.\\)")
         (ident-regex (concat qualifier-regex "*\\(\\s.+\\|\\(\\sw\\|\\s_\\)+\\)"))) ; note * for many qualifiers
    (save-excursion
      (goto-char (+ (point) (or offset 0)))
      (when (looking-at ident-regex)
        (let ((end (match-end 0)))
          (skip-syntax-backward (if (looking-at "\\s.") "." "w_")) ;; find start of operator/variable
          (while (save-excursion
                   (and (re-search-backward (concat "\\b" qualifier-regex) (line-beginning-position) t)
                        (s-matches? (concat "^" ident-regex "$") (buffer-substring-no-properties (point) end))))
            (goto-char (match-beginning 0)))
          (list (point) end))))))


