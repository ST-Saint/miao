;;; miao-utils.el --- Meow variables  -*- lexical-binding: t; -*-

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


(defun miao--switch-state (state)
  "Switch to STATE execute 'miao-switch-state-hook unless NO-HOOK is non-nil."
  (unless (eq state (miao--current-state))
    (let ((mode (alist-get state miao-state-mode-alist)))
      (funcall mode 1))))

(defun miao--current-state ()
  miao--current-state)

(defun miao-switch-to-previous-state ()
  (miao--switch-state miao--leader-previous-state))

(defun miao--event-key (event)
  (let ((c (event-basic-type event)))
    (if (and (char-or-string-p c)
             (member 'shift (event-modifiers event)))
        (upcase c)
      c)))

(defun miao--parse-input-event (event)
  (cond
   ((equal event 32)
    "SPC")
   ((characterp event)
    (string event))
   ((equal 'tab event)
    "TAB")
   ((equal 'return event)
    "RET")
   ((equal 'backspace event)
    "DEL")
   ((equal 'escape event)
    "ESC")
   ((symbolp event)
    (format "<%s>" event))
   (t nil)))

(defun miao-leader-self-insert ()
  "Default command when leader state is enabled."
  (interactive)
  (setq this-command last-command)
  (when-let ((event (miao--event-key last-input-event))
             (key (miao--parse-input-event event)))
    (push (cons 'literal key) miao--leader-keys)
    ;; Try execute if the input is valid.
    (miao--leader-try-execute)))

(defun miao--leader-lookup-key (keys)
  (let* ((overriding-local-map miao-leader-state-keymap)
         (keybind (key-binding keys)))
    keybind))

(defun miao--leader-try-execute ()
  "Try execute command.

If there is a command available on the current key binding,
try replacing the last modifier and try again."
  (let* ((key-str (miao--leader-format-keys nil))
         (cmd (miao--leader-lookup-key (read-kbd-macro key-str))))
    (cond
     ((commandp cmd t)
      (setq current-prefix-arg miao--prefix-arg
            miao--prefix-arg nil)
      (let ((miao--leader-this-command cmd))
        (miao-leader-quit)
        (setq real-this-command cmd
              this-command cmd)
        (call-interactively cmd)))
     ((keymapp cmd)
      t)
     ((equal 'control (caar miao--leader-keys))
      (setcar miao--leader-keys (cons 'literal (cdar miao--leader-keys)))
      (miao--leader-try-execute))
     (t
      (setq miao--prefix-arg nil)
      (miao-leader-quit)))))

(defun miao--leader-format-single-key (key)
  "Return a display format for input KEY."
  (cl-case (car key)
    (meta (format "M-%s" (cdr key)))
    (control (format "C-%s" (miao--leader-format-upcase (cdr key))))
    (both (format "C-M-%s" (miao--leader-format-upcase (cdr key))))
    (literal (cdr key))))

(defun miao--leader-format-upcase (k)
  "Return S-k for upcase k."
  (let ((case-fold-search nil))
    (if (and (stringp k)
             (string-match-p "^[A-Z]$" k))
        (format "S-%s" (downcase k))
      k)))

(defun miao--leader-format-keys (&optional prompt)
  "Return a display format for current input keys."
  (let ((result ""))
    (setq result
          (thread-first
            (mapcar #'miao--leader-format-single-key miao--leader-keys)
            (reverse)
            (string-join " ")))
    (cond
     ;; (miao--use-both
     ;;  (setq result
     ;;        (if (string-empty-p result)
     ;;            "C-M-"
     ;;          (concat result " C-M-"))))
     ;; (miao--use-meta
     ;;  (setq result
     ;;        (if (string-empty-p result)
     ;;            "M-"
     ;;          (concat result " M-"))))
     ;; (miao--use-literal
     ;;  (setq result (concat result " ○")))

     (prompt
      (setq result (concat result " C-"))))
    result))


(provide 'miao-utils)
