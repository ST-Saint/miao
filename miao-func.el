;;; miao-func.el --- Meow variables  -*- lexical-binding: t; -*-

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


(defun miao-append ()
  (interactive)
  (miao-insert-mode t)
  (unless (eolp)
    (forward-char)))

(defun miao-insert-begin ()
  (interactive)
  (beginning-of-line-text)
  (miao-insert-mode))

(defun miao-insert-end ()
  (interactive)
  (end-of-line)
  (miao-insert-mode))

(defun miao-leader-quit ()
  "miao leader state quit and switch to previous state"
  (interactive)
  (miao-switch-to-previous-state)
  (setq miao--leader-previous-state nil)
  (setq miao--leader-keys nil)
  (setq overriding-local-map nil))

(defun miao-mark-word ()
  (interactive)
  (let ((bound (bounds-of-thing-at-point 'word)))
    (goto-char (car bound))
    (set-mark (cdr bound))))

(defun miao-mark-symbol ()
  (interactive)
  (let ((bound (bounds-of-thing-at-point 'symbol)))
      (goto-char (car bound))
      (set-mark (cdr bound))))

(defun miao-mark-string ()
  (interactive)
  (let ((bound (bounds-of-thing-at-point 'string)))
    (goto-char (car bound))
    (set-mark (cdr bound))))

(defun miao-mark-string-inner ()
  (interactive)
  (let ((bound (bounds-of-thing-at-point 'string)))
    (goto-char (+ 1 (car bound)))
    (set-mark (- (cdr bound) 1))))

(defun miao-mark-list-inner ()
  (interactive)
  (if (and (region-active-p)
           (looking-back "[\(\[\{]"))
      (backward-char))
  (let ((begin
         (if (region-active-p)
             (condition-case nil
                 (or (backward-up-list 1 t t) (point))
                 (error (point)))
           (cond ((looking-back "[\])\{]") (backward-list))
                 ((looking-at "[\(\[\{]") (point))
                 (t (or (backward-up-list 1 t t)) (point)))))
        (end (scan-sexps (point) 1)))
    (set-mark (- end 1))
    (goto-char (+ 1 begin))))

(defun miao-mark-list ()
  (interactive)
  (let ((begin (if (region-active-p)
                   (or (backward-up-list 1 t t) (point))
                   (cond ((looking-back "[\])\{]") (backward-list))
                    ((looking-at "[\(\[\{]") (point))
                    (t (or (backward-up-list 1 t t)) (point)))))
        (end (scan-sexps (point) 1)))
    (set-mark end)
    (goto-char begin)))

(defun miao-toggle-mark-point ()
  (interactive)
  (if (region-active-p)
      (exchange-point-and-mark)))

(defun miao-delete-char ()
  (interactive)
  (delete-char 1))

(defun miao-quit-window ()
  (interactive)
  (quit-window))

(defun miao-mark-line ()
  (interactive)
  (if (region-active-p)
      (if (equal (point) (region-end))
          (progn (next-logical-line) (end-of-line))
        (progn (previous-logical-line) (beginning-of-line)))
    (let ((pos (point))
        (begin (pos-bol))
        (end (pos-eol)))
     (goto-char end)
    (set-mark begin))))

(defun miao-cursor-blink ()
  (interactive)
  (let ((pos (point))
        (begin (pos-bol))
        (end (pos-eol)))
    (goto-char begin)
    (set-mark (+ 1 end))
    (run-with-idle-timer 0.5 nil (lambda () (progn (deactivate-mark) (goto-char pos))))))

(defun miao-setup-modeline ()
  "Setup indicator appending the return of function
`miao-indicator' to the modeline.

This function should be called after you setup other parts of the mode-line
 and will work well for most cases.

If this function is not enough for your requirements,
use `miao-indicator' to get the raw text for indicator
and put it anywhere you want."
  (unless (cl-find '(:eval (miao-indicator)) mode-line-format :test 'equal)
    (setq-default mode-line-format (append '((:eval (miao-indicator))) mode-line-format))))

(provide 'miao-func)
