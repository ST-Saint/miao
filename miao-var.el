;;; miao-var.el --- Miao variables  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Internal variables and customizable variables.

;;; Code:


(defvar-local miao--current-state nil
  "A symbol represent current state.")

(defvar-local miao--leader-previous-state nil
  "A symbol represent current state.")

(defvar miao--leader-keys nil)
(defvar miao--prefix-arg nil)

(defvar miao-modeline-indicators '((normal . "N")
                                   (insert . "I")
                                   (leader . "L")
                                   (bypass . "B")))

(defvar miao-leader-major-keymap-hash
  #s(hash-table))

(defvar miao-leader-mode-keys
  ;; ! → / (33, 47)
  ;; 0 → 9 (48, 57)
  ;; : → @ (58, 64)
  ;; A -> Z
  ;; ^ -> `
  ;; a -> z
  ;; { -> ~
  (mapcar
   (lambda (c)
     (if (numberp c)
       (char-to-string c)
       c))
   (append (number-sequence ?! ?/)
           (number-sequence ?0 ?9)
           (number-sequence ?: ?@)
           (number-sequence ?A ?Z)
           (number-sequence ?^ ?`)
           (number-sequence ?a ?z)
           (number-sequence ?\{ ?~)
           '("<up>" "<down>" "<left>" "<right>"))))

(defvar miao-bypass-mode-list
  '(calc-mode
    dired-mode
    pdf-view-mode
    magit-status-mode
    ediff-mode
    mu4e-main-mode
    mu4e-headers-mode))

(defvar miao-bypass-keymap-hash
  #s(hash-table))

(defvar miao-bypass-mode-keys
  (append (number-sequence ?! ?/)
          (number-sequence ?0 ?9)
          (number-sequence ?: ?@)
          (number-sequence ?A ?Z)
          (number-sequence ?^ ?`)
          (number-sequence ?a ?z)
          (number-sequence ?\{ ?~)))

(defface miao-modeline-face
  '((t :weight bold))
  "Normal state indicator."
  :group 'miao)

(provide 'miao-var)
