;;; miao.el --- Miao editing -*- lexical-binding: t; -*-

;; Author: Yayu Wang
;; Keywords: convenience, modal-editing
;; Package-Requires: ((emacs "30.0"))
;; Version: 1.0.0
;; URL: https://www.github.com/st-saint/miao
;;
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

;;; Code:

(make-variable-buffer-local
 (defvar foo-count 0
   "Number of foos inserted into the current buffer."))

(defvar miao-state-mode-alist
  '((normal . miao-normal-mode)
    (insert . miao-insert-mode)
    (keypad . miao-keypad-mode))
  "Alist of miao states -> modes")

(defun insert-foo ()
  (interactive)
  (setq foo-count (1+ foo-count))
  (insert "foo"))

(defvar-local miao--current-state nil
  "A symbol represent current state.")

(defvar-local miao--keypad-previous-state nil
  "A symbol represent current state.")

(defvar miao--keypad-keys nil)
(defvar meow--prefix-arg nil)

(defmacro miao--state-mode-p (name)
  `(defun ,(miao--intern name "-p") ()
     (bound-and-true-p ,(miao--intern name))
       )
  )

(defun miao--intern (name &optional p)
  (intern (concat "miao-" (symbol-name name) "-mode" p)))


(defun miao--set-cursor-type (type)
  (if (display-graphic-p)
      (setq cursor-type type)
    (let* ((shape (or (car-safe type) type))
           (param (cond ((eq shape 'bar) "6")
                        ((eq shape 'hbar) "4")
                        (t "2"))))
      (send-string-to-terminal (concat "\e[" param " q")))))

(define-minor-mode miao-mode
  "Get your foos in the right places."
  :lighter " Miao"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c ESC") 'miao-normal-mode)
	    (define-key map (kbd "<space>") 'miao-keypad-mode)
            map)
  (if miao-mode
      (progn
	(setq miao--current-state 'normal)
	(miao-normal-mode t)
	(message "enable normal mode")
	)
    (miao--disable-current-mode)))

(defun miao--disable-current-mode ()
  (when miao--current-state
    (message "disable: %s" (miao--intern miao--current-state))
    (funcall (miao--intern miao--current-state) -1))
  )

(defvar miao-normal-state-keymap
  (let ((keymap (make-sparse-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "C-c f") 'insert-foo)
    (define-key keymap (kbd "i") 'miao-insert-mode)
    (define-key keymap (kbd "a") 'miao-insert-mode)
    (define-key keymap (kbd "j") 'next-line)
    (define-key keymap (kbd "k") 'previous-line)
    (define-key keymap (kbd "h") 'left-char)
    (define-key keymap (kbd "l") 'right-char)
    (define-key keymap (kbd "<SPC>") 'miao-keypad-mode)
    keymap)
  "Keymap for Miao normal state.")

(defvar miao-keypad-leader-keymap
  (let ((keymap (make-sparse-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "b r") 'revert-buffer)
    (define-key keymap (kbd "<SPC>") 'beacon-blink)
    keymap)
  "Keymap for Miao normal state.")

(define-minor-mode miao-normal-mode
  "Get your foos in the right places."
  :lighter " ಎ·ω·ಎ"
  :keymap miao-normal-state-keymap
  (message "before normal %s %s" miao-normal-mode miao--current-state)
  (if miao-normal-mode
      (if (not (equal miao--current-state 'normal))
	  ;; switch to normal mode: disable current + set normal
	  (progn
	    (miao--disable-current-mode)
	    (setq miao--current-state 'normal)))
    (setq miao--current-state nil)
    )
  (setq cursor-type 'box)
  (message "after normal %s" miao--current-state))

(miao--state-mode-p normal)

(defun miao-append ()
  (interactive)
  (miao-insert-mode t)
  (unless (eolp)
    (forward-char)))

(define-minor-mode miao-insert-mode
  "Get your foos in the right places."
  :lighter " /ᐠ.ꞈ.ᐟ\\"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<escape>") 'miao-normal-mode)
            map)
  (message "before insert %s" miao--current-state)
  (if miao-insert-mode
      (if (not (equal miao--current-state 'insert))
	  ;; switch to insert mode: disable current + set insert
	  (progn
	    (miao--disable-current-mode)
	    (setq miao--current-state 'insert)))
    (setq miao--current-state nil)
    )
  (setq cursor-type 'bar)
  (message "after insert %s" miao--current-state))

(miao--state-mode-p insert)


(define-minor-mode miao-keypad-mode
  "Get your foos in the right places."
  :lighter " ಎ-ω-ಎ"
  :keymap (let ((map (make-sparse-keymap)))
	    (suppress-keymap map t)
	    (define-key map (kbd "<escape>") 'miao-keypad-quit)
	    (define-key map [remap keyboard-quit] 'miao-keypad-quit)
	    (define-key map [remap self-insert-command] 'miao-keypad-self-insert)
            map)
  (message "before keypad %s %s" miao-keypad-mode miao--current-state)
  (setq miao--keypad-previous-state miao--current-state)
  (if miao-keypad-mode
      (if (not (equal miao--current-state 'keypad))
	  ;; switch to keypad mode: disable current + set keypad
	  (progn
	    (miao--disable-current-mode)
	    (setq miao--current-state 'keypad)))
    (setq miao--current-state nil)
    )
  (message "after keypad %s" miao--current-state))

(defun miao-keypad-quit ()
  "miao keypad state quit and switch to previous state"
  (interactive)
  (miao-switch-to-previous-state)
  (message "keypad to %s" miao--keypad-previous-state)
  (setq miao--keypad-previous-state nil)
  (setq miao--keypad-keys nil))

(defun miao--switch-state (state)
  "Switch to STATE execute 'miao-switch-state-hook unless NO-HOOK is non-nil."
  (unless (eq state (miao--current-state))
    (let ((mode (alist-get state miao-state-mode-alist)))
      (funcall mode 1))))

(defun miao--current-state ()
  miao--current-state)

(defun miao-switch-to-previous-state ()
  (miao--switch-state miao--keypad-previous-state))

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


(defun miao-keypad-self-insert ()
  "Default command when keypad state is enabled."
  (interactive)
  (setq this-command last-command)
  (when-let ((event (miao--event-key last-input-event))
             (key (miao--parse-input-event event)))
    (push (cons 'literal key) miao--keypad-keys)
    ;; Try execute if the input is valid.
    (miao--keypad-try-execute)))

(defun miao--keypad-lookup-key (keys)
  (let* ((overriding-local-map miao-keypad-leader-keymap)
         (keybind (key-binding keys)))
      keybind))

(defun miao--keypad-try-execute ()
  "Try execute command.

If there is a command available on the current key binding,
try replacing the last modifier and try again."
  (let* ((key-str (miao--keypad-format-keys nil))
         (cmd (miao--keypad-lookup-key (read-kbd-macro key-str))))
    (message "keypad-keys %s key-str: %s cmd: %s" miao--keypad-keys key-str cmd)
    (cond
     ((commandp cmd t)
      (setq current-prefix-arg miao--prefix-arg
            miao--prefix-arg nil)
      (let ((miao--keypad-this-command cmd))
        (miao-keypad-quit)
        (setq real-this-command cmd
              this-command cmd)
        (call-interactively cmd)))
     ((keymapp cmd)
      t)
     ((equal 'control (caar miao--keypad-keys))
      (setcar miao--keypad-keys (cons 'literal (cdar miao--keypad-keys)))
      (miao--keypad-try-execute))
     (t
      (setq miao--prefix-arg nil)
      (message "%s is undefined" (miao--keypad-format-keys nil))
      (miao-keypad-quit)))))

(defun miao--keypad-format-single-key (key)
  "Return a display format for input KEY."
  (message "single-key %s" key)
  (cl-case (car key)
    (meta (format "M-%s" (cdr key)))
    (control (format "C-%s" (miao--keypad-format-upcase (cdr key))))
    (both (format "C-M-%s" (miao--keypad-format-upcase (cdr key))))
    (literal (cdr key))))

(defun miao--keypad-format-upcase (k)
  "Return S-k for upcase k."
  (let ((case-fold-search nil))
    (if (and (stringp k)
             (string-match-p "^[A-Z]$" k))
        (format "S-%s" (downcase k))
      k)))

(defun miao--keypad-format-keys (&optional prompt)
  "Return a display format for current input keys."
  (let ((result ""))
    (setq result
          (thread-first
              (mapcar #'miao--keypad-format-single-key miao--keypad-keys)
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

(provide 'miao)
