;;; package --- Summary
;;; Commentary:
;;; common config
;;; Code:

;; required for helper functions
(require 'local-common)

(setq-default frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "E - %b"))))
;; (setq-default frame-title-format '("%f [%m] Emacs"))

(when (display-graphic-p)
  ;; load theme based on whether theme file is set to light or dark
  (ensure-package 'solarized-theme)
  (require 'solarized-theme)
  (let ((f (substitute-in-file-name "$XDG_RUNTIME_DIR/theme")))
	(if (file-exists-p f)
		(if (with-temp-buffer
			  (insert-file-contents (substitute-in-file-name f))
			  (goto-char (point-min))
			  (looking-at "0"))
			(load-theme 'solarized-dark t)
		  ;; default to light
		  (load-theme 'solarized-light t))(load-theme 'solarized-light t))
	  )
  )

(require 'server)
(unless (server-running-p)
  (server-start))

;;; basic defaults. these are set as values for all
;;; modes if the mode itself does not define it

;; indent with whitespace by default to make files appear the same
;; across editors
(setq-default indent-tabs-mode nil)
;; set tab width. override in mode if needed
(setq-default tab-width 4)

;; no lock files. annoying
(setq-default create-lockfiles nil)



;;; basics, Emacs native
(require 'ispell)
(require 'flyspell)
(require 'recentf)
(require 'saveplace)
(require 'tramp)

;; limit recentf, exclude unneeded
(setq-default recentf-max-saved-items 1000)
(setq-default recentf-exclude '("^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'"
                        ".*-autoloads\\.el\\'"
                        "[/\\]\\.elpa/"
						"/tmp/"
						"/ssh:"
						(concat package-user-dir "/.*-autoloads\\.el\\'")))

;; paste where mouse is located
(setq-default mouse-yank-at-point t)

(setq ;; clean startup screen
      user-full-name "Martin Kjær Jørgensen"
      initial-major-mode 'text-mode
      inhibit-startup-message t
      initial-scratch-message nil
      ;; seems disable cursor blinking.
      ;; used instead of blink-cursor-mode
      visible-cursor nil
      ;; no backup files
      backup-inhibited t
      ;; if accidentally enabled, copyx files in dirx
      backup-directory-alist `(("." . "~/.saves"))
      backup-by-copying t
      ;; not too many recentf menu items
      recentf-max-menu-items 25
      ;; default to en-us dicts and choose hunspell
      ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_US"
      ispell-really-hunspell t
      ispell-personal-dictionary "~/.ispell"
      ;; ease shutdown
      confirm-kill-emacs nil
      confirm-kill-processes nil
      ;; dont check file is readable. can slow quit
      ;; if file is on remote filesystems
      save-place-forget-unreadable-files nil
      ;; needed for hungry el packages, like lsp
      gc-cons-threshold 134217728
	  ;; always follow symlinks to source controls
	  vc-follow-symlinks t
	  ;; scroll one line at a time using keyboard
	  scroll-step 1
	  ;; keep cursor at position when scrolling pages. its annoying
	  ;; that it jumps to top or bottom of screen when scrolling pages
	  scroll-preserve-screen-position t
	  ;;
	  tramp-default-method "ssh"
	  ;; limit mini buffer size
	  ;; max-mini-window-height 0.10
      )

(when (>= emacs-major-version 27)
  (setq read-process-output-max (* 1024 1024)))

(defun set-gui-options()
  (when (display-graphic-p)
	;; disable annoying gui features
	(tool-bar-mode -1)
	(blink-cursor-mode 0)
	)
  )

(if (daemonp)
	(add-hook 'after-make-frame-functions
			  (lambda (frame)
				(set-gui-options)
				)
			  )
  )
(set-gui-options)


;; overwrite selected text
(add-hook 'after-init-hook 'delete-selection-mode)

;; auto revert
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'global-auto-revert-mode)

;;; easier dictionary switching
(global-set-key (kbd "<f8>") 'switch-dictionary)


;;; answer questions easier
(defalias 'yes-or-no-p 'y-or-n-p)

(defun after-init-flyspell-mode()
  ;;; Check the whole buffer after flyspell loads, so to see current
  ;;; spelling errors
  (flyspell-buffer)
  )


(add-hook 'flyspell-mode-hook #'after-init-flyspell-mode)
;; enable flyspell in all prog-modes only for comments and strings
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
;; annoying. especially in yml mode
;; (add-hook 'text-mode-hook 'flyspell-mode)


;; add eletric pair on all prog modes. should not be intruding any modes.
(add-hook 'prog-mode-hook 'electric-pair-mode)
;; might as well delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; have list of recent files
(recentf-mode t)
;; show line numbers
(line-number-mode t)
;; show column number
(column-number-mode t)
;; remove menu bar. must be -1 to removex
(menu-bar-mode -1)
;; remember file positions
(save-place-mode 1)
;; show matching parenthesis
(show-paren-mode 1)

;; change directory immediately to make sure it is enabled
(ispell-change-dictionary "en_US")

;; set utf-8 as default
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(require 'hideshow)

;; ediff
(require 'ediff)

(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

(defun ediff-startup-hook-setup ()
    ;; move to the first difference
    (ediff-next-difference)
    ;; move to the merged buffer window
    (winum-select-window-by-number 3)
    ;; save the windows layout
    (window-configuration-to-register ?a))

(add-hook 'ediff-startup-hook 'ediff-startup-hook-setup)


(require 're-builder)
;; Support a slightly more idiomatic quit binding in re-builder
(define-key reb-mode-map (kbd "C-c C-k") 'reb-quit)

;; --- xterm-color
;; assume we use xterm-256color
(ensure-package 'xterm-color)
(require 'xterm-color)


;;  --- bash completion
;; kinda nice in shell-mode

(ensure-package 'bash-completion)
(require 'bash-completion)

(bash-completion-setup)



(require 'eshell)
(require 'esh-mode)

;; add xterm colors workarounds to eshell
;; (add-hook 'eshell-before-prompt-hook
;;           (lambda ()
;;             (setq xterm-color-preserve-properties t)))

;; (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;; (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
(setq eshell-banner-message "")

;; commented out because it doesn't seem to have any noticeable effect
;; (setenv "TERM" "xterm-256color")

(setq-default explicit-shell-file-name "/bin/bash")


;;----------------------------------------------------------------------------
;; Nicer naming of buffers for files with identical names
;;----------------------------------------------------------------------------
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;; xterm-color config for shell mode
;; (setq comint-output-filter-functions
;;       (remove 'ansi-color-process-output comint-output-filter-functions))

;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             ;; Disable font-locking in this buffer to improve performance
;;             (font-lock-mode -1)
;;             ;; Prevent font-locking from being re-enabled in this buffer
;;             (make-local-variable 'font-lock-function)
;;             (setq font-lock-function (lambda (_) nil))
;;             (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))


;;
(add-hook 'shell-mode-hook 'rename-uniquely)
;; (add-hook 'term-mode-hook 'rename-uniquely)

;; --- ecr
(require 'erc)
(require 'erc-log)
(require 'erc-replace)
(require 'erc-autoaway)

(setq erc-log-channels-directory (expand-file-name "erc-logs" user-emacs-directory)
      erc-log-write-after-send t
      erc-log-write-after-insert t
      erc-autoaway-idle-seconds 600

      ;; logging
      erc-enable-logging t
      erc-save-buffer-on-part t
      erc-hide-list '("JOIN" "PART" "QUIT" "MODE")
	  ;; emojify chat
      erc-replace-alist
      '(
        (":+1:" . "👍")
        (":laughing:" . "😂")
        (":slightly_smiling_face:" . "😃")
        (":smiley:" . "😃")
        (":wink:" . "😉")
        ))

;; show fill column as 'piped' | lines
(setq-default indicate-buffer-boundaries 'left)
(setq-default display-fill-column-indicator-character ?\u254e)

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)


;; --- browse kill ring
;; handy tool

(ensure-package 'browse-kill-ring)
(require 'browse-kill-ring)

(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "C-M-y") 'browse-kill-ring)

(define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
(define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
(define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous)


;; --- exec-path-from-shell
(ensure-package 'exec-path-from-shell)
(require 'exec-path-from-shell)


;; --- Which-key mode
;; show keybindings in popup when typing
(ensure-package 'which-key)
(require 'which-key)

;; remove lighter from modeline
(setq which-key-lighter "")

(which-key-mode t)


;; added to aid emacs in setting environment vars
;; correct and according to user shell

;; (when (daemonp)
;;   (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
;; 	(add-to-list 'exec-path-from-shell-variables var))
;;   (exec-path-from-shell-initialize))
(when (daemonp)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
	(add-to-list 'exec-path-from-shell-variables var))
  )
(exec-path-from-shell-initialize)


;; handling large files
(require 'so-long)
(add-hook 'after-init-hook 'so-long-enable)

(ensure-package 'vlf)
(require 'vlf)

;; --- mode line bell
;; instead of sound bell. should work in terminal as well
(ensure-package 'mode-line-bell)
(require 'mode-line-bell)

;; --- beacon mode
;; highlight curson when window scrolls

(ensure-package 'beacon)
(require 'beacon)

;; remove default lighter string from mode line
(setq-default beacon-lighter "")
;; shrink the beacon a bit
(setq-default beacon-size 20)

(add-hook 'after-init-hook 'beacon-mode)

(add-hook 'after-init-hook 'mode-line-bell-mode)

;; --- basic editing
(ensure-package 'iedit)
(require 'iedit)

(global-set-key (kbd "C-c C-r") 'iedit-mode)
(setq iedit-toggle-key-default nil
      )

;; --- treemacs
;; liked by some programming modes, like Java+LSP
(ensure-package 'treemacs)
(require 'treemacs)
;; (ensure-package 'treemacs-all-the-icons)
;; (require 'treemacs-all-the-icons)
(ensure-package 'treemacs-icons-dired)
(require 'treemacs-icons-dired)


;; --- projectile
;; make emacs project aware
(ensure-package 'projectile)
(require 'projectile)

;; (when (executable-find "rg")
;;   (setq-default projectile-generic-command "rg --files --hidden"))

(ensure-package 'treemacs-projectile)
(require 'treemacs-projectile)


(setq projectile-completion-system 'ivy ;make ivy aware
      projectile-project-search-path '("~/dev/" "~/work/") ;default paths
	  ;; attempt to disable project name on modeline. this should
	  ;; speedup, also over TRAMP
	  projectile-dynamic-mode-line nil
	  ;; this should improve emacs performance when projectile is enabled
	  ;; in a buffer gotten over tramp/ssh
	  ;; projectile-file-exists-remote-cache-expire nil
      projectile-globally-ignored-directories
      (append '(
                ".git"
                ".svn"
                "bin"
                "out"
                "repl"
                "target"
                "venv"
                "build*"
                )
              projectile-globally-ignored-directories)
      )

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defadvice projectile-on (around exlude-tramp activate)
  "This should disable projectile when visiting a remote file"
  (unless  (--any? (and it (file-remote-p it))
                   (list
                    (buffer-file-name)
                    list-buffers-directory
                    default-directory
                    dired-directory))
    ad-do-it))

;; enable projectile through counsel-projectile
;; (projectile-mode t)

;; a reddit user suggested this config to make projectile not crawl
;; on remote file systems over TRAMP which can be cumbersome
;; https://www.reddit.com/r/emacs/comments/320cvb/projectile_slows_tramp_mode_to_a_crawl_is_there_a/
(defadvice projectile-project-root (around ignore-remote first activate)
      (unless (file-remote-p default-directory) ad-do-it))

;; --- sorting and filtering algorithm for other packages to use
(ensure-package 'prescient)
(require 'prescient)

(prescient-persist-mode 1)

;; --- company
;; basic completion
(ensure-package 'company)
(require 'company)
(require 'company-dabbrev)
(require 'company-dabbrev-code)
(require 'company-ispell)
(ensure-package 'company-prescient)
(require 'company-prescient)

(setq company-selection-wrap-around t   ;wrap around
      company-minimum-prefix-length 3 ;shorter prefix
	  company-dabbrev-ignore-case t
	  company-lighter-base "" 			; remove lighter
	  company-dabbrev-code-ignore-case t
	  company-idle-delay 0.2 			; speed up completion
	  company-dabbrev-code-other-buffers 'all
	  company-backends '((company-keywords company-capf company-dabbrev-code company-dabbrev company-ispell company-files))
	  company-global-modes '(not comint-mode erc-mode help-mode gud-mode)
	  company-dabbrev-downcase nil)	  ; make dabbrev completions case sensitive

(add-hook 'company-mode-hook 'company-prescient-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'conf-mode-hook 'company-mode)


;; --- yasnippet
;; useful to have and recommended by LSP defaults
(ensure-package 'yasnippet)
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


;; --- flycheck
;; needs to be 'latest' >31 for LSP mode to
;; work properly
(ensure-package 'flycheck)
(require 'flycheck)


;; Wait idle seconds before running flycheck
(setq flycheck-idle-change-delay 2
	  ;;
	  flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
	  ;; jump to next error instead of warning or info
      flycheck-navigation-minimum-level 'error
	  flycheck-checker-error-threshold 100
	  )

(add-hook 'prog-mode-hook 'flycheck-mode)



;;; --- emacs lisp
(setq ;;; have flycheck use emacs load-path when searching
      ;;; for packages
      flycheck-emacs-lisp-load-path load-path
      )


;; --- common lisp
(ensure-package 'slime)
(require 'slime)
(ensure-package 'slime-company)
(require 'slime-company)

(setq slime-company-completion 'fuzzy
	  inferior-lisp-program "sbcl"
      slime-company-after-completion 'slime-company-just-one-space)

(slime-setup '(slime-fancy slime-repl slime-fuzzy slime-company))

(add-to-list 'slime-lisp-implementations
			 '(sbcl ("sbcl") :coding-system utf-8-unix))

;; From http://bc.tech.coop/blog/070515.html
(defun lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

(define-key lisp-mode-map (kbd "C-c l") 'lispdoc)

;; --- wayland
;; (setq wl-copy-process nil)
;; (defun wl-copy (text)
;;   (setq wl-copy-process (make-process :name "wl-copy"
;;                                       :buffer nil
;;                                       :command '("wl-copy" "-f" "-n")
;;                                       :connection-type 'pipe))
;;   (process-send-string wl-copy-process text)
;;   (process-send-eof wl-copy-process))
;; (defun wl-paste ()
;;   (if (and wl-copy-process (process-live-p wl-copy-process))
;;       nil ; should return nil if we're the current paste owner
;;     (shell-command-to-string "wl-paste -n | tr -d \r")))
;; (setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)

;;; --- xclip
;;; interact with X11 clipboard
(ensure-package 'xclip)
(require 'xclip)

(unless (display-graphic-p)
  ;; only needed in terminal mode
  (xclip-mode 1)
)


;;; Ivy/Counsel/Swiper
;; narrowing framework Ivy
(ensure-package 'ivy)
(require 'ivy)
;; additions to Ivy
(ensure-package 'ivy-rich)
(require 'ivy-rich)
;; xref intergration with ivy
(ensure-package 'ivy-xref)
(require 'ivy-xref)
;; parts of ivy
(ensure-package 'counsel)
(require 'counsel)
;; fast textbuffer searching
(ensure-package 'swiper)
(require 'swiper)
;; alternative to amx/smex etc.
(ensure-package 'ivy-prescient)
(require 'ivy-prescient)


(define-key global-map [menu] nil)
;; such a common everyday function. bind it also to something simpler
(define-key global-map (kbd "M-o") 'other-window)

;; as per recommendation from ivy-rich website
(setq ivy-use-virtual-buffers t
      ivy-rich-path-style 'abbrev 		; or 'full
      ivy-count-format "(%d/%d) "
	  ;; make prompt selectable. for instance, while saving buffer to a new
	  ;; file and not wanting to select any of the suggestions
	  ivy-use-selectable-prompt t
      enable-recursive-minibuffers t
	  ;; using these options, try fix slowdown when switching buffers
	  ;; with switch-to-buffer while having tramp sessions running
	  ivy-rich-parse-remote-buffer nil
	  ivy-rich-parse-remote-file-path nil
	  )
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;; xref initialization is different in Emacs 27 - there are two different
;; variables which can be set rather than just one
(when (>= emacs-major-version 27)
  (setq xref-show-definitions-function #'ivy-xref-show-defs))
;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
;; commands other than xref-find-definitions (e.g. project-find-regexp)
;; as well
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)


;; enable ivy and rich mode
(ivy-mode 1)
(ivy-rich-mode 1)

;; used to add history to minibuffer selections like M-x
;; instead of smex which produces compile errors on install
(ivy-prescient-mode 1)

;; replace default keys
(define-key global-map [remap isearch-forward] #'swiper)
(define-key global-map [remap execute-extended-command] #'counsel-M-x)


;; --- gnus
(require 'gnus)
(require 'gnus-agent)
(require 'gnus-score)
(require 'gnus-cache)
(require 'mml)
(require 'nndraft)
(require 'nnfolder)
(require 'smtpmail)
(require 'cl-macs)

(defvar my:smtp-relays
  '((:gotu "mail.gotu.dk" starttls "submission")
    (:cephalopo  "mail.cephalopo.net" starttls "submission")
    (:cdk "asmtp.yousee.dk" starttls "submission"))
  "Relays Hosts to use depending on From: when sending mail.")

(defvar my:default-smtp-relay
  '("mail.cephalopo.net" starttls "submission")
  "Default relay host.")

(defvar my:force-using-default-smtp nil
  "When not nil, for using dim:default-smtp-relay")

(defun my:message-smtpmail-send-it ()
  "Automatically adjust the SMTP parameters to match the From header.
   thanks to https://github.com/dimitri/emacs.d
  "
  (let* ((from    (message-field-value "From"))
		 (network (cond
				   ((string-match-p "gotu.dk" from) :gotu)
				   ((string-match-p "cephalopo.net" from) :cephalopo)
				   ((string-match-p "c.dk" from) :cdk))))
    ;; get connection details from my:smtp-relays
    (cl-destructuring-bind (smtpmail-smtp-server
						 smtpmail-stream-type
						 smtpmail-smtp-service)
						(if my:force-using-default-smtp
							my:default-smtp-relay
						  (or (cdr (assoc network my:smtp-relays)) my:default-smtp-relay))
						(message-use-send-mail-function))))


(setq gnus-directory "~/.emacs.d/gnus/" ; gnus new home
	  ;; do not read or write to .newsrc. we dont plan to use other
	  ;; reader along with gnus
      gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil
	  ;; set new cache directories
	  gnus-cache-directory (concat gnus-directory "cache/")
      gnus-cache-active-file (concat gnus-directory "cache/active") ; no slash! this is a file, not a directory!
	  ;; move gnus related files into its dir
      gnus-article-save-directory (concat gnus-directory "save/")
      gnus-kill-files-directory (concat gnus-directory "killfiles/")
      gnus-message-archive-group "Sent"
      gnus-agent-directory (concat gnus-directory "agent/")
	  ;; show result of sign verification from 'mm-verify-option
	  ;; show alternatives as buttons, like text/html
      gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed" "multipart/alternative")
	  ;; mark sent/Gcc mails as read
	  gnus-gcc-mark-as-read t
	  ;; show more headers
      gnus-extra-headers (quote (To Cc Newsgroups))
	  ;; not novice anymore, we think
	  gnus-novice-user nil
	  ;; silent exit
      gnus-interactive-exit nil
	  ;; sort by date primarily
	  gnus-thread-sort-functions (quote (gnus-thread-sort-by-most-recent-date gnus-thread-sort-by-most-recent-number))
	  ;; gnus-summary-line-format ":%U%R %B %s %-60=|%4L |%-20,20f |%&user-date; \n"
      gnus-summary-line-format "%U%R%z %&user-date; %I%(%[%4L: %-20,20n%]%) %S\n"
	  ;; set date format
	  gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M"))
      mail-user-agent (quote gnus-user-agent)
      message-directory "~/.emacs.d/gnus/Mail/"
      message-fill-column 78
      message-wash-forwarded-subjects t
	  ;; kill message after sending it
	  message-kill-buffer-on-exit t
	  ;; dont read alias expansions from .mailrc
	  message-mail-alias-type nil
	  ;; use 'smtpmail' to send messages
	  message-send-mail-function 'my:message-smtpmail-send-it
	  ;; message-send-mail-function (quote message-smtpmail-send-it)
	  ;; warn if sending to an invalid email address
      message-setup-hook (quote (message-check-recipients))
	  ;; determine the value of MFT headers. use built in gnus functions
	  message-subscribed-address-functions (quote (gnus-find-subscribed-addresses))
	  ;; extended citation line
	  message-citation-line-function (quote message-insert-formatted-citation-line)
	  message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"
      ;; cursor position 'above' 'below' 'traditional' (inline)
	  ;; newsgroups frown upon anything but inline
      message-cite-reply-position (quote above)
      ;; dont autosave
      message-auto-save-directory nil
	  send-mail-function (quote smtpmail-send-it)
	  ;; debug sending email
      smtpmail-debug-info t
	  smtpmail-stream-type 'starttls
      mm-default-directory "~/dwl"
      mm-tmp-directory "~/tmp"
	  ;; verify known signed parts
      mm-verify-option (quote known)
      mm-decrypt-option (quote known)
	  ;; wait plain text when viewing
	  mm-discouraged-alternatives (quote ("text/html" "text/richtext" "image/.*"))
	  ;; use same extra headers
      nnmail-extra-headers gnus-extra-headers
      nndraft-directory (concat message-directory "drafts/")
      nnfolder-directory (concat message-directory "archive/")
      nnfolder-active-file (concat message-directory "archive")
      )


(add-hook 'mail-mode-hook 'footnote-mode)
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-mode-hook 'turn-on-flyspell)
(add-hook 'message-mode-hook 'turn-on-auto-fill)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'message-mode-hook 'footnote-mode)


;; --- bbdb
(ensure-package 'bbdb)
(require 'bbdb)

;; enable bbdb in needed packages
(bbdb-initialize (quote (gnus message)))


;; --- counsel-projectile
;; Counsel-projectile provides further ivy integration into projectile
;; by taking advantage of ivy's support for selecting from a list of
;; actions and applying an action without leaving the completion
;; session
(ensure-package 'counsel-projectile)
(require 'counsel-projectile)
(ensure-package 'counsel-tramp)
(require 'counsel-tramp)

(define-key global-map (kbd "C-c s") 'counsel-tramp)

;; no need to reassign keys with counsel/ivy aware
;; alternatives. counsel-projectile-mode does that.

;; dont add counsel-projectile to projectile-mode-hook. lisp max depth
;; errors occurs
(add-hook 'prog-mode-hook 'counsel-projectile-mode)
(add-hook 'conf-mode-hook 'counsel-projectile-mode)
(add-hook 'text-mode-hook 'counsel-projectile-mode)

;; --- indent tools
;; Smarter way to indent code
(ensure-package 'indent-tools)
(require 'indent-tools)

;; enable for all programming modes
(add-hook 'prog-mode-hook #'indent-tools-minor-mode)


;; --- git
;; Git
;; basic tools for handling git files and git repos
(ensure-package 'magit)
(require 'magit)
(ensure-package 'treemacs-magit)
(require 'treemacs-magit)

;; (add-hook 'git-commit-mode-hook #'turn-on-flyspell)
;; (add-hook 'git-commit-mode-hook 'turn-on-auto-fill)

;; most for fun/convenience
(ensure-package 'git-timemachine)
(require 'git-timemachine)


(ensure-package 'gitattributes-mode)
(require 'gitattributes-mode)

(ensure-package 'gitconfig-mode)
(require 'gitconfig-mode)

(ensure-package 'gitignore-mode)
(require 'gitignore-mode)
(ensure-package 'gitignore-templates)
(require 'gitignore-templates)

;; show diff highlights in buffer
(ensure-package 'diff-hl)
(require 'diff-hl)

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'text-mode-hook 'diff-hl-mode)


;; lsp-mode
;; LSP compatibility
(ensure-package 'lsp-mode)
(require 'lsp-clients)
(require 'lsp-completion)
(require 'lsp-diagnostics)
(ensure-package 'lsp-treemacs)
(require 'lsp-treemacs)

(setq lsp-eldoc-render-all t
	  lsp-file-watch-threshold 10000 	; we're handling big projects
	  lsp-keymap-prefix "C-c C-l"
	  lsp-completion-enable-additional-text-edit t)


;; enable lsp-ui for more fancy UI features, like docs and flycheck errors
;; shown in buffer
;; (ensure-package 'lsp-ui)
;; (require 'lsp-ui)
;; disable lsp diagnostics (flycheck) for now.
;; it sets lsp as sole or default flycheck provider
;; and makes errors when idle if enabled.
;; no use for it anyway for now.
;; (setq lsp-diagnostics-provider :flycheck)
;; (setq lsp-auto-configure nil)

(define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
(define-key lsp-mode-map (kbd "C-c a") #'lsp-execute-code-action)
(define-key lsp-mode-map (kbd "C-c C-i") #'lsp-format-buffer)
(define-key lsp-mode-map (kbd "C-c C-v r") #'lsp-rename)

(ensure-package 'dap-mode)
(require 'dap-mode)

;; sessions locals breakpoints expressions controls tooltip
;; disable dap ui controls
(setq dap-auto-configure-features '(sessions locals breakpoints expressions))

;; --- golang
(ensure-package 'go-mode)
(require 'go-mode)

(defun init-go-mode()
  ;; go-vet disabled because its command "go tool vet" is deprecated
  ;; on newer golang platforms
  (setq flycheck-disabled-checkers '(go-vet go-test))
  (setq-local projectile-globally-ignored-directories
              ;; 'vendor' dir is made by go modules
              (append '("vendor") projectile-globally-ignored-directories))
  )

(add-hook 'go-mode-hook #'init-go-mode)
(add-hook 'go-mode-hook #'lsp)



;; --- groovy mode
(ensure-package 'groovy-mode)
(require 'groovy-mode)

(add-hook 'groovy-mode-hook #'lsp)


;; --- python mode
(ensure-package 'elpy)
(require 'elpy)

;; replace flymake with flycheck
;; (when (load "flycheck" t t)
;;   )

;; rpc needs its own virtualenv
(setq elpy-rpc-virtualenv-path "~/.virtualenvs/elpyrpc3"
	  ;; disable annoying auto elpy config
	  elpy-modules (delq 'elpy-module-flymake (delq 'elpy-module-company elpy-modules)))

;; remap to standard format key stroke
(define-key elpy-mode-map (kbd "C-c C-i") 'elpy-format-code)

(defun init-elpy-mode()
  ;; it seems we need to disable checkers on elpy-mode-hook, otherwise
  ;; they are re-enabled. disable flake8 and pycompile

  ;; dabbrev in comments is nice
  (setq-local company-dabbrev-code-everywhere t
			  company-idle-delay 0.1)
  (setq-local flycheck-disabled-checkers (quote (python-flake8 python-pycompile)))
  (setq-local company-backends '((elpy-company-backend company-dabbrev-code)))
  )

(elpy-enable)

(add-hook 'elpy-mode-hook #'superword-mode) ; become snake-case aware
(add-hook 'elpy-mode-hook #'init-elpy-mode)


;; --- web-mode
(ensure-package 'web-mode)
(require 'web-mode)
(ensure-package 'company-web)
(require 'company-web)
(require 'company-web-html)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(flycheck-add-mode 'html-tidy 'web-mode)

(setq web-mode-markup-indent-offset 2
	  web-mode-css-indent-offset 2
	  web-mode-code-indent-offset 4
	  web-mode-enable-auto-pairing t
	  web-mode-enable-auto-closing t
	  web-mode-enable-auto-opening t
	  web-mode-enable-auto-indentation t
	  web-mode-enable-comment-interpolation t
	  web-mode-enable-heredoc-fontification t
	  ;; these two highlights seems to mess up when in tty
	  web-mode-enable-current-element-highlight nil
	  web-mode-enable-current-column-highlight nil
      web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
	  )


(defun init-web-mode()
  (set (make-local-variable 'company-backends)
       '((company-web-html :with company-dabbrev-code :with
                       company-yasnippet)
         company-capf company-files))
  (when (string-equal "jsx" (file-name-extension buffer-file-name))
    (setup-tide-mode))
  )

(add-hook 'web-mode-hook #'init-web-mode)




;;; SASS and SCSS
(ensure-package 'scss-mode)
(require 'scss-mode)

(setq-default scss-compile-at-save nil)


;; Skewer CSS
(ensure-package 'skewer-mode)
(require 'skewer-mode)

(add-hook 'css-mode-hook 'skewer-css-mode)


;;; LESS
(ensure-package 'less-css-mode)
(require 'less-css-mode)
(ensure-package 'skewer-less)
(require 'skewer-less)

(add-hook 'less-css-mode-hook 'skewer-less-mode)






;; --- typescript/javascript tide mode
(ensure-package 'tide)
(require 'tide)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(define-key tide-mode-map (kbd "C-c C-i") 'tide-format)


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (tide-hl-identifier-mode t))

;; configure javascript-tide checker to run after your default javascript checker
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

;; configure jsx-tide checker to run after your default jsx checker
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; configure jsx-tide checker to run after your default jsx checker
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; add tide mode to web-mode when editing
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)



;; --- php mode
(ensure-package 'php-mode)
(require 'php-mode)
(ensure-package 'company-php)
(require 'company-php)

;; not available anymore?
;; (ensure-package 'phpcbf)
;; (require 'phpcbf)
;; (define-key php-mode-map (kbd "C-c C-i") 'phpcbf)

(defun init-php-mode()
  ;; enable to navigate camelCase words smarter
  (subword-mode 1)
  ;; disable phpcs and phpmd for now until configured. produces too
  ;; many errors for flycheck to handle.
  ;; (setq-local flycheck-disabled-checkers '(php-phpcs php-phpmd))
  ;; optional
  (ac-php-core-eldoc-setup)
  ;;
  ;; (setq-local company-dabbrev-ignore-case nil)
  ;; (make-local-variable company-backends)
  (setq-local company-backends '((company-ac-php-backend company-files company-keywords company-capf company-dabbrev-code company-dabbrev)))
  )

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-hook 'php-mode-hook 'init-php-mode)


;; --- lua mode
(ensure-package 'lua-mode)
(require 'lua-mode)
(ensure-package 'company-lua)
(require 'company-lua)

;; add luarock files as well
(add-to-list 'auto-mode-alist '("\\.rockspec" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.busted" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.slua" . lua-mode))

(defun init-lua-mode()
  ;;; combine lua and dabbrev in one completion, so if lua fails dabbrev
  ;;; can provide
  (setq-local company-backends '((company-lua :with company-dabbrev-code :with company-yasnippet)
         company-capf company-files))
  )

(add-hook 'lua-mode-hook #'init-lua-mode)


;; --- json mode
(ensure-package 'json-mode)
(require 'json-mode)
(ensure-package 'counsel-jq)
(require 'counsel-jq)

(defun init-json-mode()
  ;;; combine lua and dabbrev in one completion, so if lua fails dabbrev
  ;;; can provide
  (setq-local company-backends '((company-dabbrev company-ispell)))
  )


(add-to-list 'auto-mode-alist '("\\.json" . json-mode))

(define-key json-mode-map (kbd "C-c C-i") 'json-mode-beautify)

(add-hook 'json-mode-hook 'counsel-projectile-mode)
(add-hook 'json-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'company-mode)


;; --- highlight indentation mode
;; used for yaml mailny
(ensure-package 'highlight-indentation)
(require 'highlight-indentation)


;; --- yaml mode
(ensure-package 'yaml-mode)
(require 'yaml-mode)

(defun init-yaml-mode()
  ;; tabs are outlawed
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local highlight-indentation-offset 2)
  (flycheck-select-checker 'yaml-yamllint)
  )

(add-hook 'yaml-mode-hook #'init-yaml-mode)
(add-hook 'yaml-mode-hook 'indent-tools-minor-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-mode)
(add-hook 'yaml-mode-hook 'counsel-projectile-mode)
(add-hook 'yaml-mode-hook 'flycheck-mode)
(add-hook 'yaml-mode-hook 'company-mode)

(add-to-list 'auto-mode-alist '("\\.yamllint" . yaml-mode))


;; --- k8s mode
(ensure-package 'k8s-mode)
(require 'k8s-mode)

(setq k8s-indent-offset nil)

(add-hook 'k8s-mode-hook #'yas-minor-mode)


;; --- xml
(require 'nxml-mode)
(ensure-package 'xml-format)
;; (require 'xml-format)

;; seems broken. takes precedence over other backends
;; (require 'company-xsd)

(defun init-nxml-mode()
  (setq-local company-backends '((company-dabbrev company-ispell)))
  (setq-local company-minimum-prefix-length 3)
  )

(add-hook 'nxml-mode-hook #'init-nxml-mode)
(add-hook 'nxml-mode-hook 'counsel-projectile-mode)
(add-hook 'nxml-mode-hook 'flycheck-mode)
(add-hook 'nxml-mode-hook 'company-mode)
(add-hook 'nxml-mode-hook 'hs-minor-mode)

(add-to-list 'hs-special-modes-alist
             (list 'nxml-mode
                   "<!--\\|<[^/>]*[^/]>"
                   "-->\\|</[^/>]*[^/]>"
                   "<!--"
                   'nxml-forward-element
                   nil))

(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(setq nxml-slash-auto-complete-flag t)

;; (define-key nxml-mode-map (kbd "C-c C-i") #'nxml-pretty-format)
(define-key nxml-mode-map (kbd "C-c C-i") #'xml-format-buffer)
;; (define-key nxml-mode-map (kbd "C-c C-i") #'tidy-buffer-xml)
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.scxml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sch\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rng\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.svg\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rss\\'" . nxml-mode))


;; --- markdown
(ensure-package 'markdown-mode)
(require 'markdown-mode)
(ensure-package 'markdown-preview-mode)
(require 'markdown-preview-mode)

;; use perl markdown
(setq markdown-command "multimarkdown")


(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; use github markdown for readmes
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


;; --- docker
(ensure-package 'docker)
(require 'docker)
(ensure-package 'docker-tramp)
(require 'docker-tramp)


;; --- dockerfile
(ensure-package 'dockerfile-mode)
(require 'dockerfile-mode)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


;; --- nginx mode
(ensure-package 'nginx-mode)
(require 'nginx-mode)

(ensure-package 'company-nginx)
(require 'company-nginx)

(add-hook 'nginx-mode-hook #'company-nginx-keywords)
(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))


;; --- systemd mode
(ensure-package 'systemd)
(require 'systemd)


;; --- plantuml mode

(ensure-package 'plantuml-mode)
(require 'plantuml-mode)
;; (ensure-package 'flycheck-plantuml)
;; (require 'flycheck-plantuml)

;; (add-to-list 'flycheck-checkers 'plantuml)
;; (flycheck-add-mode 'plantuml 'plantuml-mode)

(setq plantuml-default-exec-mode 'jar)

(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))


;; --- java mode
(ensure-package 'lsp-java)
(require 'lsp-java)

(defun init-java-mode()
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)

  )

(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook #'subword-mode) ;navigate camelcase easier
(add-hook 'java-mode-hook #'init-java-mode)

(define-key java-mode-map (kbd "C-c C-l") nil)
(setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.68.0/jdt-language-server-0.68.0-202101202016.tar.gz")


;; --- nov reader
;; for epub files

(ensure-package 'nov)
(require 'nov)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))



;; --- csv mode

(ensure-package 'csv-mode)
(require 'csv-mode)

(setq csv-separators '("," ";" "|" " "))

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))



;; --- rainbow mode
;; nice to have
(ensure-package 'rainbow-mode)
(require 'rainbow-mode)
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))


;; --- rainbow delimiters
;; color delimiters differently
(ensure-package 'rainbow-delimiters)
(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)



;; --- crontab mode
(ensure-package 'crontab-mode)
(require 'crontab-mode)

(add-to-list 'auto-mode-alist '("\\.?cron\\(tab\\)?\\'" . crontab-mode))


;; --- highlight escape sequence mode
;;

(ensure-package 'highlight-escape-sequences)
(require 'highlight-escape-sequences)

(add-hook 'prog-mode-hook 'turn-on-hes-mode)


;; --- meson mode
(ensure-package 'meson-mode)
(require 'meson-mode)


;; --- cmake mode
(ensure-package 'cmake-mode)
(require 'cmake-mode)


;; --- sql mode
(ensure-package 'sqlformat)
(require 'sqlformat)
(require 'sql)

(add-hook 'sql-mode-hook 'sqlformat-mode)

;; redefine key
(define-key sql-mode-map (kbd "C-c C-f") nil)
(define-key sql-mode-map (kbd "C-c C-i") 'sqlformat)


;; --- latex auctex
(ensure-package 'auctex)
(require 'latex)
(ensure-package 'company-auctex)
(require 'company-auctex)

(setq TeX-PDF-mode t
	  TeX-source-correlate-method (quote synctex)
	  TeX-source-correlate-mode t
      TeX-source-correlate-start-server t)

(defun init-latex-mode()
  (setq-local company-backends '((company-auctex company-ispell company-dabbrev))
  ;; complete on anything
  ;; (setq-local company-minimum-prefix-length 1)
	   )
  )

(add-hook 'TeX-mode-hook #'init-latex-mode)


;; ---- rg
;; tried rg.el but it had many user questions and could not
;; open remote file over TRAMP automatically from the result list

(ensure-package 'deadgrep)
(require 'deadgrep)

(define-key global-map (kbd "M-s r") 'deadgrep)


;; --- diminish
;; cosmetic purposes
(ensure-package 'diminish)
(require 'diminish)

(diminish 'abbrev-mode)
(diminish 'hs-minor-mode)
(diminish 'rainbow-mode)
(diminish 'ivy-mode)

(provide 'emacs.common)
;;; emacs.common.el ends here
