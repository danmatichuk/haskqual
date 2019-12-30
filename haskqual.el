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

(defvar haskqual-last-tags-file nil)
(defvar haskqual-global-tags-file nil)

(defvar-local haskqual-tags-file nil)

(defvar-local haskqual-file-checked nil)

;; Functions for tracking which TAGS file is most appropriate for the given file

;; Copied from https://www.emacswiki.org/emacs/EtagsSelect
(defun --find-tags-file (path name)
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (if (buffer-file-name)
      (catch 'found-it
        (--find-tags-file-r path name))
    (error "buffer is not visiting a file")))

(defun --find-tags-file-r (path name)
   "find the tags file from the parent directories"
   (let* ((parent (file-name-directory path))
          (possible-tags-file (concat parent name)))
     (cond
       ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
       ((string= (concat "/" name) possible-tags-file) (error "no tags file found"))
       (t (--find-tags-file-r (directory-file-name parent) name)))))

(defun haskqual-auto-update-tags-table ()
  "Automatically update the TAGS file based on the last TAGS file visited."
  (interactive)
  (unless haskqual-file-checked
    (setq haskqual-file-checked t)
    (if haskqual-tags-file
      (haskqual-set-tags-file haskqual-tags-file)
      (if haskqual-last-tags-file
          (progn
            (if (haskqual-check-tags-current haskqual-last-tags-file)
              (haskqual-set-tags-file haskqual-last-tags-file)
              (haskqual-find-tags-file)))
        (progn
          (haskqual-find-tags-file))))))

(defun with-tags-table (tags-file cont)
  (let* ((old-tags-table-list tags-table-list)
         (old-tags-file-name tags-file-name))
    (progn
      (--set-tags-file tags-file)
      (prog1
          (funcall cont)
        (setq tags-table-list old-tags-table-list)
        (setq tags-file-name old-tags-file-name)))))

(defun haskqual-set-tags-file (tags-file)
  "Switch to the given TAGS file."
  (interactive)
   (progn
     (setq haskqual-tags-file tags-file)
     (setq haskqual-last-tags-file tags-file)
     (--set-tags-file tags-file)))

(defun --set-tags-file (tags-file)
  (progn
    (setq tags-table-list (list tags-file))
    (setq tags-file-name tags-file)))

(defun haskqual-find-tags-file ()
  "Switch to the nearest TAGS file if found"
  (interactive)
  (haskqual-next-tags-file-from (concat (buffer-file-name) "/.")))

(defun haskqual-next-tags-file ()
  (interactive)
  (if haskqual-tags-file
      (haskqual-next-tags-file-from haskqual-tags-file)
    (haskqual-find-tags-file)))

(defun haskqual-next-tags-file-from (tags-file)
  "Switch to the next nearest TAGS file if found"
  (interactive)
  (when tags-file
    (let* ((updir (directory-file-name (file-name-directory tags-file)))
           (next-tags-file (ignore-errors (--find-tags-file updir "TAGS"))))
      (if next-tags-file
        (if (haskqual-check-tags-current next-tags-file)
            (haskqual-set-tags-file next-tags-file)
          (haskqual-next-tags-file-from next-tags-file))
        (message "No suitable TAGS file found.")))))


(defun haskqual-check-tags-current (tags-file)
  "Check if the given TAGS file defines this module"
  (with-tags-table tags-file
    (lambda () (let* ((module-name (haskqual-parse-module-name))
           (xrefs (when module-name (xref--get-definitions '(etags--xref-backend) module-name))))
      (seq-some (lambda (xref) (string= (buffer-file-name (xref--buffer-of xref)) (buffer-file-name (current-buffer)))) xrefs)))))


;; Functions for resolving qualified module lookups

(defun haskqual-parse-module-name ()
  "Parse a haskell buffer for the module name"
  (save-excursion
      (beginning-of-buffer)
      (re-search-forward
       "^\\s-*module\\s-*\\([A-Za-z0-9.]*\\)" nil :no-error)
      (match-string-no-properties 1)))

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

(defun haskqual-parse-imports ()
  "Parse a haskell buffer for module imports."
  (save-excursion
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
      dict)))

(defun haskqual-parse-exports ()
  "Parse a haskell buffer for module exports"
  (save-excursion
    (beginning-of-buffer)
    (let ((dict nil)
          (sep "\\(?:\\s-\\|\n\\)*\\(?:--.*\n\\)?\\(?:#.*\n\\)?\\(?:\\s-\\|\n\\)*")
          )
      (while (re-search-forward
              (concat "[,(]"
                      sep
                      "module"
                      sep
                      "\\([A-Za-z0-9.]*\\)")
              nil :no-error)
        (push (match-string-no-properties 1) dict))
      dict)))

(defun haskqual-expand-qualifier (qualname)
  "Expand local qualifiers and module exports for a qualified identifier, producing a list of possible expansions."
  (let ((quals))
    (dolist (elt (haskqual-parse-imports) quals)
      (when (or (and qualname (string= qualname (haskqual-import-name elt))) ;; is a fully qualified module
             (or
               (and (not qualname) (not (haskqual-import-qualified elt))) ;; unqualified identifier can match any unqualified import
               (and qualname (string= qualname (haskqual-import-requalifier elt))))) ;; qualifier matches "as"
        (push (haskqual-import-name elt) quals)))))
        

(defun haskqual-get-xref (ident)
  "Search for an identifer by expanding it into possible fully-qualified variants"
  (when ident
    (let ((indent-xrefs (haskqual-congruent-xrefs ident)))
      (if indent-xrefs indent-xrefs
        (let* ((match (string-match "^\\(\\(?:[A-Za-z0-9.]+\\)+\\)[.]\\(\\(?:\\sw\\|\\s_\\|\\s.\\)+\\)" ident))
               (qualname (when match (match-string 1 ident)))
               (basename (if match (match-string 2 ident) ident))
               (base-xrefs (when match (haskqual-congruent-xrefs basename))))
          (if base-xrefs base-xrefs
            (haskqual--search-qualifiers qualname basename)))))))


(defun haskqual--search-qualifiers (qualname basename)
  (let* ((qualifiers (haskqual-expand-qualifier qualname))
         (qualified-idents (--map (concat it "." basename) qualifiers))
         (modulename (haskqual-parse-module-name))
         (allqualifiers (if qualname qualified-idents (cons (concat modulename "." basename) qualified-idents)))
         (xref-result (xref--get-first-xref allqualifiers)))
    (if xref-result xref-result
      (let ((exports))
        (dolist (elt qualifiers exports)
          (let* ((module-xref (nth 0 (xref--get-definitions '(etags--xref-backend) elt)))
                 (module-exports (when module-xref (with-current-buffer (xref--buffer-of module-xref) (haskqual-parse-exports)))))
            (setq exports (append module-exports exports))))
        (let ((xrefs (xref--get-first-xref (--map (concat it "." basename) exports))))
          (if xrefs xrefs
            (xref--get-definitions '(etags--xref-backend) basename)))))))

(defun haskqual-congruent-xrefs (ident)
  "Return xrefs for the given identifier iff they all belong to the same file."
  (let* ((xrefs (xref--get-definitions '(etags--xref-backend) ident)))
    (if (car xrefs)
        (let* ((buf (buffer-file-name (xref--buffer-of (car xrefs)))))
          (if (seq-every-p (lambda (elt) (string= (buffer-file-name (xref--buffer-of elt)) buf)) (cdr xrefs))
              xrefs
            nil)
          ))))

(defun haskqual-find-definition ()
  (interactive)
  (let ((xrefs (haskqual-get-xref (haskqual-ident-at-point))))
    (if xrefs (xref--show-xrefs xrefs nil) (message "No definitions found."))))

(defun haskqual-find-definition-other-window ()
  (interactive)
  (let ((xrefs (haskqual-get-xref (haskqual-ident-at-point))))
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
  (let ((reg (haskqual-ident-pos-at-point)))
    (when reg
      (apply #'buffer-substring-no-properties reg))))

;; Copied from dante
(defun haskqual-ident-pos-at-point (&optional offset)
  "Return the span of the (qualified) identifier at point+OFFSET, or nil if none found."
  (let* ((qualifier-regex "\\([[:upper:]][[:alnum:]]*\\.\\)")
         (ident-regex (concat qualifier-regex "*\\(\\(\\sw\\|\\s_\\|\\s.\\)+\\)"))) ; note * for many qualifiers
    (save-excursion
      (goto-char (+ (point) (or offset 0)))
      (when (looking-at ident-regex)
        (let ((end (match-end 0)))
          (skip-syntax-backward "w_.") ;; find start of operator/variable
          (while (save-excursion
                   (and (re-search-backward (concat "\\b" qualifier-regex) (line-beginning-position) t)
                        (s-matches? (concat "^" ident-regex "$") (buffer-substring-no-properties (point) end))))
            (goto-char (match-beginning 0)))
          (list (point) end))))))


;; Functions for falling back to a global TAGS file (i.e. for manual identifier lookups)


(defun xref-global-find-tag (tag)
  (interactive (list (xref-global--read-identifier-tags)))
  (xref-global--with-global-tags
   (lambda ()
     (let ((xrefs (xref--get-definitions '(etags--xref-backend) tag)))
       (xref--show-xrefs xrefs nil)))))

;; Try finding the global TAGS file if we can, falling back to the local one
(defun xref-global--with-global-tags (cont)
  (progn
    (setq global-tags haskqual-global-tags-file)
    (unless global-tags
      (setq global-tags (ignore-errors (--find-tags-file (buffer-file-name) "TAGS-global"))))
    (unless global-tags
      (setq global-tags haskqual-tags-file))
    (with-tags-table global-tags cont)))

;; Copied from xref.el
(defun xref--global-read-identifier (prompt)
  "Return the identifier at point or read it from the minibuffer."
  (let* ((backend (xref-find-backend))
         (id (xref-backend-identifier-at-point backend)))
    (cond ((or current-prefix-arg
               (not id)
               (xref--prompt-p this-command))
           (completing-read (if id
                                (format "%s (default %s): "
                                        (substring prompt 0 (string-match
                                                             "[ :]+\\'" prompt))
                                        id)
                              prompt)
                            (xref-global--with-global-tags (lambda () (xref-backend-identifier-completion-table backend)))
                            nil nil nil
                            'xref--read-identifier-history id))
          (t id))))

(defun xref-global--read-identifier-tags ()
  (let* ((xref-backend-functions '(etags--xref-backend)))
    (xref--global-read-identifier "Find definitions of: ")))
