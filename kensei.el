 ;;; kensei.el --- Emacs extension for email management

 ;; Copyright (C) 2012  Thomas Kjeldahl Nilsson

 ;; Author: thomas@kjeldahlnilsson.net
 ;; Keywords: email client extension

 ;; This file is free software; you can redistribute it and/or modify
 ;; it under the terms of the GNU General Public License as published by
 ;; the Free Software Foundation; either version 2, or (at your option)
 ;; any later version.

 ;; This file is distributed in the hope that it will be useful,
 ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ;; GNU General Public License for more details.

 ;; You should have received a copy of the GNU General Public License
 ;; along with GNU Emacs; see the file COPYING.  If not, write to
 ;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 ;; Boston, MA 02111-1307, USA.

;;; Version and dependencies

(defun kensei-version ()
  (interactive)
  (message "0.0.2"))

(load-file "dash.el")
(load-file "s.el")

;;; Set up mode(s) for kensei

;;; References for emacs mode stuff:
;;; http://renormalist.net/Renormalist/EmacsLanguageModeCreationTutorial
;;; http://ergoemacs.org/emacs/elisp_menu_for_major_mode.html

(defvar kensei-mode-hook nil)
(defvar kensei-folder-list-mode-hook nil)
(defvar kensei-email-list-mode-hook nil)
(defvar kensei-current-email-mode-hook nil)

(defun kensei-mode ()
  "The global, common kensei mode"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'kensei-mode)
  (setq mode-name "kensei")

  (defvar kensei-mode-map nil "Global keymap for kensei mode")

   ;; keybindings
  (setq kensei-mode-map (make-sparse-keymap))
  (define-key kensei-mode-map (kbd "q") 'kensei-quit)
  (define-key kensei-mode-map (kbd "w") '(lambda () (interactive)(message "todo: write email")))
  (define-key kensei-mode-map (kbd "w") '(lambda () (interactive)(message "todo: write email")))
  (define-key kensei-mode-map (kbd "c") '(lambda () (interactive)(message "todo: write email")))
  (define-key kensei-mode-map (kbd "C") '(lambda () (interactive)(message "todo: write email")))
  (define-key kensei-mode-map (kbd "n") '(lambda () (interactive)(message "todo: write email")))
  (define-key kensei-mode-map (kbd "N") '(lambda () (interactive)(message "todo: write email")))
  (define-key kensei-mode-map (kbd "m") '(lambda () (interactive)(message "todo: write email")))
  (define-key kensei-mode-map (kbd "M") '(lambda () (interactive)(message "todo: write email")))

  ;; menu bar items
  (define-key kensei-mode-map [menu-bar] (make-sparse-keymap))
  (let ((menuMap (make-sparse-keymap "Kensei")))
    (define-key kensei-mode-map [menu-bar kensei] (cons "Kensei" menuMap))
    (define-key menuMap [write-email]
      '("Write email" . kensei-version))
    (define-key menuMap [separator]
      '("--"))
    (define-key menuMap [quit]
      '("Quit kensei" . kensei-quit))
    (define-key menuMap [separator]
      '("--"))
    (define-key menuMap [version]
      '("Current version" . kensei-version)))

  (use-local-map kensei-mode-map)
  (run-hooks 'kensei-mode-hook))

(defun kensei-folder-list-mode ()
  (kensei-mode)
  ;; Window-local keybindings
  (local-set-key "\t" '(lambda () (interactive)(message "todo: cycle visibility")))
  (local-set-key (kbd "<RET>") '(lambda () (interactive)(message "todo: choose folder")))
  (setq minor-mode 'kensei-folders))

(defun kensei-email-list-mode ()
  (kensei-mode)
  ;; Window-local keybindings
  (local-set-key "\t" '(lambda () (interactive)(message "todo: tab=cycle visiblity of replies")))
  (local-set-key (kbd "<RET>") '(lambda () (interactive)(message "todo: enter/click=show current mail")))
  (local-set-key "d" '(lambda () (interactive)(message "todo: d=mark for deletion")))
  (local-set-key "D" '(lambda () (interactive)(message "todo: D=delete mail now")))
  (local-set-key "a" '(lambda () (interactive)(message "todo: a=mark for archiving")))
  (local-set-key "A" '(lambda () (interactive)(message "todo: A=archive mail now")))
  (local-set-key "t" '(lambda () (interactive)(message "todo: t = mark for tagging")))
  (local-set-key "T" '(lambda () (interactive)(message "todo: T = tag mail now (prompt for tag/folder)")))
  (local-set-key "x" '(lambda () (interactive)(message "todo: x=execute marked actions")))
  (local-set-key "r" '(lambda () (interactive)(message "todo: r=reply")))
  (local-set-key "R" '(lambda () (interactive)(message "todo: R=reply to all")))
  (local-set-key "f" '(lambda () (interactive)(message "todo: f=forward")))
  ;; TODO set local menu bar items
  (setq minor-mode 'kensei-emails))

(defun kensei-current-email-mode ()
  (kensei-mode)
  ;; Window-local keybindings
  (local-set-key "\t" '(lambda () (interactive)(message "todo: tab=cycle visiblity of replies")))
  (local-set-key "d" '(lambda () (interactive)(message "todo: delete this")))
  (local-set-key "D" '(lambda () (interactive)(message "todo: delete this")))
  (local-set-key "a" '(lambda () (interactive)(message "todo: archive this")))
  (local-set-key "A" '(lambda () (interactive)(message "todo: archive this")))
  (local-set-key "t" '(lambda () (interactive)(message "todo: tag this")))
  (local-set-key "T" '(lambda () (interactive)(message "todo: tag this")))
  (local-set-key (kbd "<RET>") '(lambda () (interactive)(message "todo: reply")))
  (local-set-key "r" '(lambda () (interactive)(message "todo: r=reply")))
  (local-set-key "R" '(lambda () (interactive)(message "todo: R=reply to all")))
  (local-set-key "f" '(lambda () (interactive)(message "todo: f=forward")))
  ;; TODO set local menu bar items
  (setq minor-mode 'kensei-email))

;;; Setup and teardown of app state

(defconst kensei-folder-list-buffer-name "*kensei-folders*")
(defconst kensei-email-list-buffer-name "*kensei-emails*")
(defconst kensei-current-email-buffer-name "*kensei-email*")
(defconst kensei-exceptions "*kensei-exceptions*")

(defun kensei-all-windows-readonly (switch)
  (save-window-excursion
    (switch-to-buffer kensei-folder-list-buffer-name)
    (toggle-read-only switch)
    (switch-to-buffer kensei-email-list-buffer-name)
    (toggle-read-only switch)
    (switch-to-buffer kensei-current-email-buffer-name)
    (toggle-read-only switch)))

(defun kensei-init-windows ()
  (kensei-all-windows-readonly -1)
  ;; create left window = folders
  (switch-to-buffer kensei-folder-list-buffer-name)
  (kensei-folder-list-mode)
  ; create top right window = emails
  (split-window-right nil)
  (other-window 1)
  (switch-to-buffer kensei-email-list-buffer-name)
  (window-resize nil (- (truncate (* 0.7 (frame-width))) (window-width)) t)
  (kensei-email-list-mode)
  ; create bottom right window = current email
  (split-window-below nil)
  (other-window 1)
  (switch-to-buffer kensei-current-email-buffer-name)
  (kensei-current-email-mode)
  ; set up initial state
  (save-window-excursion
    (kensei-update-folder-window)
    ;;;TODO show email list for first/default selected folder
    )
  (kensei-all-windows-readonly t))

(defun kensei-start ()
  "Save window configuration, set up kensei"
  (interactive)
  ;;; TODO don't run if already in kensei mode
  ;;; TODO check that dependencies are fulfilled, otherwise exit with helpful install instructions
  (window-configuration-to-register :pre-kensei-fullscreen)
  (delete-other-windows)
  (kensei-init-windows))

(defun kensei-quit ()
  "Restores the previous window configuration and kills kensei buffers"
  (interactive)
  (kill-buffer kensei-folder-list-buffer-name)
  (kill-buffer kensei-email-list-buffer-name)
  (kill-buffer kensei-current-email-buffer-name)
  (jump-to-register :pre-kensei-fullscreen))

;;; Logic specific to each of the main kensei windows


(defun kensei-update-folder-window ()
  (switch-to-buffer kensei-folder-list-buffer-name)
  (erase-buffer)
  (let* ((accounts (kensei-fetch-account-list)))
    (insert (s-join "\n" accounts))))

(defun kensei-result-has-exception (json-blob)
  (assoc 'exception json-blob))

(defun kensei-update-email-list ()
  (switch-to-buffer kensei-email-list-buffer-name)
  (erase-buffer)
  (insert (kensei-fetch-emails "Gmail" "INBOX")))

(defun kensei-show-email ()
  (switch-to-buffer kensei-current-email-buffer-name)
  (erase-buffer)
  (insert (kensei-fetch-email "Gmail" "INBOX" "1234")))

;; Support setting appearance, event-lambda-handling, model properties

(defun kensei-set-property! (property-name property-value str)
  "Set arbitrary property on given string"
  (put-text-property 0 (length str) property-name property-value str)
  str)

(defun kensei-set-face! (face-property str)
  "Set specified font-face-lock property on given string"
  (kensei-set-property! 'font-lock-face face-property str)
  str)

(defun kensei-set-onkey! (trigger-key str event-lambda)
  "Set event-handling function for given key input event on given string"
  (let ((map (make-sparse-keymap)))
    (define-key map trigger-key event-lambda)
    (kensei-set-property! 'keymap map str))
  str)

;; Example of setting a property 'uuid on a string, then setting a click event
;;    that accesses value of the uuid property on of that string when clicked.

;; (setq wat "WAT")
;; (kensei-set-property! 'uuid 3333 wat)
;; (kensei-set-onkey! [mouse-1] wat (lambda (event)
;; 				   (interactive "e")
;; 				   (let ((window (posn-window (event-end event)))
;; 					 (pos (posn-point (event-end event)))
;; 					 file)
;; 				     (message "%s" (get-text-property pos 'uuid)))))
;; (insert wat)WATWAT WAT  WAT;;; note: after inserting the string, it will only have the last set uuid, not any new ones set on the wat referenced string

;; Backend/model interface

(defun kensei-backend-version ()
  (interactive)
  (message (kensei-backend 'version ())))

(defun kensei-fetch-folders (account-id)
  (kensei-backend 'folder-list (list account-id)))

(defun kensei-fetch-emails (account-id folder-name)
  (kensei-backend 'get-emails-in (list account-id folder-name)))

(defun kensei-fetch-email (account-id folder-name uid)
  (kensei-backend 'get-email (list account-id folder-name uid)))

(defun kensei-fetch-account-list ()
  (kensei-backend 'account-idsd (list)))

(defun kensei-add-gmail-account ()
  "Add offlineimap and msmtp config for a Gmail account"
  (interactive)
  (let ((address (read-string "Gmail email address: " "YOURUSERNAME@gmail.com"))
	(password (read-string "Gmail password: " ""))
	(account-id (read-string "Name of the account: " "personal-gmail-account")))
    (kensei-backend 'add-gmail-account (list address password account-id))))

;;; TODO add background version of synch command? (Current one will freeze emacs until done)
(defun kensei-synchronize ()
  "Perform offlineimap synchronization of all accounts"
  (interactive)
  (message "Synchronizing mail folders...")
  (message (kensei-backend 'synchronize-now ()))
  (message "Synchronization done."))

(require 'json)
(json-encode t)
(setq kensei-use-mock-backend nil) ;; Set to true in test suite

;; JSON EXAMPLES
;; Parsing
;;  (json-read-from-string "[true, 4.5]")
;; (json-read-from-string "{\"one\":\"one\ntwo\nthree\",\"two\":true}")
;; Encoding
;;  (json-encode '(1 2 3)) ;;"[1, 2, 3]"
;;  (json-encode '(:foo 1 :bar 2 :baz 3)) ;;"{\\"foo\\":1, \\"bar\\":2, \\"baz\\":3}"

;;; TODO add option to turn on debugging: will dump api calls/params and the result of the call

(defun kensei-backend (command param-list)
  "Interface to the backend, all backend returns are json strings"
  (let* ((binary (if kensei-use-mock-backend
		     "~/versioncontrolled/public/kensei/kensei-mock-backend.rb"
		   "kensei"))
	 (command (s-snake-case (symbol-name command)))
	 (params (s-join " " param-list))
	 (shell-result (shell-command-to-string (concat binary " " command " " params)))
	 (json-array-type 'list)
	 (result-json (json-read-from-string shell-result)))
    (kensei-indicate-any-api-exception result-json)
    result-json))

(defun kensei-indicate-any-api-exception (json-with-exception)
  "Standard presentation of kensei errors in whatever buffer is active at the time"
  (if (listp json-with-exception)
      (let ((exception-in-data (cdr (assoc 'exception json-with-exception))))
	(if exception-in-data
	    (message exception-in-data)))))

;; Tell Emacs that kensei is open for business

(provide 'kensei)
