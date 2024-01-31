;;; miao-var.el --- Meow variables  -*- lexical-binding: t; -*-

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

(defvar miao-state-mode-alist
  '((normal . miao-normal-mode)
    (insert . miao-insert-mode)
    (leader . miao-leader-mode)
    (bypass  . miao-bypass-mode))
  "Alist of miao states -> modes")

(defvar miao-keymap-alist
  `((normal . ,miao-normal-state-keymap)
    (insert . ,miao-insert-state-keymap)
    (leader . ,miao-leader-state-keymap)
    (bypass  . ,miao-bypass-state-keymap))
  "Alist of symbols of state names to keymaps.")

(defface miao-modeline-face
  '((t :foreground "unspecified" :background "unspecified" :weight bold))
  "Normal state indicator."
  :group 'miao)

(provide 'miao-var)
