;;; miao-func.el --- Miao variables  -*- lexical-binding: t; -*-

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
  (if miao-leader-mode
      (progn
        (miao-switch-to-previous-state)
        (setq miao--leader-previous-state nil)
        (setq miao--leader-keys nil)
        (setq overriding-local-map nil))))

(defun miao-mark-word ()
  (interactive)
  (when-let ((bound (bounds-of-thing-at-point 'word)))
    (goto-char (cdr bound))
    (set-mark (car bound))))

(defun miao-mark-symbol ()
  (interactive)
  (when-let ((bound (bounds-of-thing-at-point 'symbol)))
    (goto-char (cdr bound))
    (set-mark (car bound))))

(defun miao-mark-string ()
  (interactive)
  (when-let ((bound (bounds-of-thing-at-point 'string)))
    (goto-char (cdr bound))
    (set-mark (car bound))))

(defun miao-mark-string-inner ()
  (interactive)
  (when-let ((bound (bounds-of-thing-at-point 'string)))
    (goto-char (- (cdr bound) 1))
    (set-mark (+ 1 (car bound)))))

(defun miao-mark-list-inner ()
  (interactive)
  (let ((forward (or (not (region-active-p))
                     (equal (point) (region-end)))))
    (if (region-active-p)
        (if forward
            (forward-char)
          (backward-char)))
    (let ((begin
           (if (region-active-p)
               (condition-case nil
                   (or (backward-up-list 1 t t) (point))
                 (error (goto-char (- (region-beginning) (if forward 1 0)))))
             (cond ((looking-back "[\])}]") (backward-list))
                   ((looking-at "[\(\[\{]") (point))
                   (t (or (backward-up-list 1 t t)) (point)))))
          (end (scan-sexps (point) 1)))
      (if forward
          (progn
            (goto-char (- end 1))
            (set-mark (+ begin 1)))
        (goto-char (+ begin 1))
        (set-mark (- end 1))))))

(defun miao-mark-list ()
  (interactive)
  (let ((begin (if (region-active-p)
                   (or (backward-up-list 1 t t) (point))
                 (cond ((looking-back "[\])\{]") (backward-list))
                       ((looking-at "[\(\[\{]") (point))
                       (t (or (backward-up-list 1 t t)) (point)))))
        (end (scan-sexps (point) 1)))
    (goto-char end)
    (set-mark begin)))

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

(defun miao-delete-window ()
  (interactive)
  (delete-window))

(defun miao-mark-line ()
  (interactive)
  (if global-display-line-numbers-mode
      (remove-hook 'deactivate-mark-hook (lambda () (display-line-numbers-mode -1)))
    (display-line-numbers-mode t)
    (add-hook 'deactivate-mark-hook (lambda () (display-line-numbers-mode -1))))
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
    (run-with-idle-timer 0.5
                         nil
                         (lambda ()
                           (if (region-active-p)
                             (progn
                               (deactivate-mark)
                               (goto-char pos)))))))

(defun miao-next-region-item ()
  (if (region-active-p)
      (let ((forward (if (equal (point) (region-end)) t nil))
            (length (- (region-end) (region-beginning)))
            (re (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end)))))
        (goto-char (if forward (region-end) (region-beginning)))
        (setq next (re-search-forward re nil t (if forward 1 -1)))
        (set-mark (+ (point) (if forward (- length) length))))))

(defun miao-next-symbol-item (direction)
  (if (not (region-active-p))
      (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                  (begin (car bounds))
                  (end (cdr bounds))
                  (offset (- (point) begin))
                  (re (concat "\\_<" (regexp-quote (buffer-substring-no-properties begin end)) "\\_>")))
        (setq case-fold-search nil)
        (re-search-forward re nil t direction))))

(defun miao-next-item ()
  (interactive)
  (if (region-active-p)
      (miao-next-region-item)
    (miao-next-symbol-item 1)))

(defun miao-prev-item ()
  (interactive)
  (if (region-active-p)
      (miao-next-region-item)
    (miao-next-symbol-item -1)))

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
