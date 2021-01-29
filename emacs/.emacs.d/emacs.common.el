;;; package --- Summary
;;; Commentary:
;;; common config
;;; Code:

;; required for helper functions
(require 'local-common)


;; load theme
(ensure-package 'zenburn-theme)
(require 'zenburn-theme)

;;; Code:
(load-theme 'zenburn t)


;;; basic defaults. these are set as values for all
;;; modes if the mode itself does not define it

(setq-default indent-tabs-mode t ;; indent with tabs
	      ;; set tab width. override in mode if needed
	      tab-width 4
              )

;;; basics, Emacs native
(require 'ispell)
(require 'flyspell)
(require 'recentf)
(require 'saveplace)

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
      )
(when (>= emacs-major-version 27)
  (setq read-process-output-max (* 1024 1024)))


(if (daemonp)
	(add-hook 'after-make-frame-functions
			  (lambda (frame)
				;; (select-frame frame)
				;; (if (display-graphic-p frame)
				;; 	(load-theme 'zenburn t)
				;;   (load-theme 'zenburn t)
				;;   )
				(when (display-graphic-p)
				  ;; disable annoying gui features
				  (tool-bar-mode -1)
				  (blink-cursor-mode 0)
				  )

				)
			  )
  ;; (load-theme 'solarized-light t)
  )
				(when (display-graphic-p)
				  ;; disable annoying gui features
				  (tool-bar-mode -1)
				  (blink-cursor-mode 0)
				  )


;;; easier dictionary switching
(global-set-key (kbd "<f8>") 'switch-dictionary)


;;; answer questions easier
(defalias 'yes-or-no-p 'y-or-n-p)

(defun after-init-flyspell-mode()
  ;;; Check the whole buffer after flyspell loads, so to see current
  ;;; spelling errors
  (flyspell-buffer)
  )

(defun init-prog-mode()
  (setq-local company-backends '(company-capf company-dabbrev-code company-files company-yasnippet))
  )

(add-hook 'flyspell-mode-hook #'after-init-flyspell-mode)
;; enable flyspell in all prog-modes only for comments and strings
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
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


;; --- xterm-color
;; assume we use xterm-256color
(ensure-package 'xterm-color)
(require 'xterm-color)

(require 'eshell)
(require 'esh-mode)

;; add xterm colors workarounds to eshell
(add-hook 'eshell-before-prompt-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))

(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
;; commented out because it doesn't seem to have any noticeable effect
(setenv "TERM" "xterm-256color")

;; add xterm colors workarounds to compilation buffers
;; Warning: this might break rg.el and ag.el
(require 'compile)

(setq compilation-environment '("TERM=xterm-256color"))

(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;; xterm-color config for shell mode
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))


;; --- exec-path-from-shell
(ensure-package 'exec-path-from-shell)
(require 'exec-path-from-shell)


;; --- rainbow mode
;; nice to have
(ensure-package 'rainbow-mode)
(require 'rainbow-mode)

;; added to aid emacs in setting environment vars
;; correct and according to user shell
(when (daemonp)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  )


;; --- basic editing
(ensure-package 'iedit)
(require 'iedit)

(global-set-key (kbd "C-c C-r") 'iedit-mode)
(setq iedit-toggle-key-default nil
      )


;; --- projectile
;; make emacs project aware
(ensure-package 'projectile)
(require 'projectile)

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

;; enable globally so that keymap becomes usable from the start
(projectile-mode t)

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
(ensure-package 'company-prescient)
(require 'company-prescient)

(setq company-selection-wrap-around t   ;wrap around
      company-minimum-prefix-length 2 ;shorter prefix
	  company-dabbrev-code-ignore-case t
	  company-dabbrev-code-other-buffers 'all
	  company-backends '((company-files company-keywords company-capf company-dabbrev-code company-dabbrev))
	  company-global-modes '(not comint-mode erc-mode help-mode gud-mode)
	  company-dabbrev-downcase nil)	  ; make dabbrev completions case sensitive

(add-hook 'company-mode-hook 'company-prescient-mode)

(add-hook 'after-init-hook 'global-company-mode)

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
      ;; jump to next error instead of warning or info
      flycheck-navigation-minimum-level 'error
	  flycheck-checker-error-threshold 100
	  )

(add-hook 'after-init-hook 'global-flycheck-mode)


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

(slime-setup '(slime-company))

(setq inferior-lisp-program "sbcl")


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

;; no need to reassign keys with counsel/ivy aware
;; alternatives. counsel-projectile-mode does that.

;; dont add counsel-projectile to projectile-mode-hook. lisp max depth
;; errors occurs
(add-hook 'after-init-hook 'counsel-projectile-mode)

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

;; (add-hook 'git-commit-mode-hook #'turn-on-flyspell)
;; (add-hook 'git-commit-mode-hook 'turn-on-auto-fill)


(ensure-package 'gitattributes-mode)
(require 'gitattributes-mode)

(ensure-package 'gitconfig-mode)
(require 'gitconfig-mode)

(ensure-package 'gitignore-mode)
(require 'gitignore-mode)



;; lsp-mode
;; LSP compatibility
(ensure-package 'lsp-mode)
(require 'lsp-clients)
(require 'lsp-diagnostics)
;; enable lsp-ui for more fancy UI features, like docs and flycheck errors
;; shown in buffer
(ensure-package 'lsp-ui)
;; disable lsp diagnostics (flycheck) for now.
;; it sets lsp as sole or default flycheck provider
;; and makes errors when idle if enabled.
;; no use for it anyway for now.
(setq lsp-diagnostics-provider :flycheck)

(define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
(define-key lsp-mode-map (kbd "C-c C-i") #'lsp-format-buffer)
(define-key lsp-mode-map (kbd "C-c C-v r") #'lsp-rename)




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


;; --- python mode
(ensure-package 'elpy)
(require 'elpy)

(elpy-enable)


;; replace flymake with flycheck
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  )

;; rpc needs its own virtualenv
(setq elpy-rpc-virtualenv-path "~/.virtualenvs/elpyrpc3")
;; remap to standard format key stroke
(define-key elpy-mode-map (kbd "C-c C-i") 'elpy-format-code)

(defun init-elpy-mode()
  ;; it seems we need to disable checkers on elpy-mode-hook, otherwise
  ;; they are re-enabled. disable flake8 and pycompile
  (setq flycheck-disabled-checkers (quote (python-flake8 python-pycompile))
		)
  )

(add-hook 'elpy-mode-hook 'init-elpy-mode)



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






;; --- typescript/javascript tide mode
(ensure-package 'tide)
(require 'tide)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(define-key tide-mode-map (kbd "C-c C-i") 'tide-format)


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (tide-hl-identifier-mode t))

;; configure javascript-tide checker to run after your default javascript checker
(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

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
(ensure-package 'phpcbf)
(require 'phpcbf)
(ensure-package 'company-php)
(require 'company-php)

(define-key php-mode-map (kbd "C-c C-i") 'phpcbf)

(defun init-php-mode()
  ;; enable to navigate camelCase words smarter
  (subword-mode 1)
  ;; disable phpcs and phpmd for now until configured. produces too
  ;; many errors for flycheck to handle.
  (setq-local flycheck-disabled-checkers '(php-phpcs php-phpmd))
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
  (set (make-local-variable 'company-backends)
       '((company-lua :with company-dabbrev-code :with
                       company-yasnippet)
         company-capf company-files))
  )

(add-hook 'lua-mode-hook #'init-lua-mode)


;; --- json mode
(ensure-package 'json-mode)
(require 'json-mode)

(add-to-list 'auto-mode-alist '("\\.json" . json-mode))

(define-key json-mode-map (kbd "C-c C-i") 'json-mode-beautify)

;; --- yaml mode
(ensure-package 'yaml-mode)
(require 'yaml-mode)

;; (add-hook 'yaml-mode-hook #'lsp)
(add-hook 'yaml-mode-hook #'indent-tools-minor-mode)


;; --- xml
(require 'nxml-mode)
(ensure-package 'xml-format)
(require 'xml-format)

(defun init-nxml-mode()
  (set (make-local-variable 'company-backends)
       '((company-nxml :with company-dabbrev-code)
         company-files))
  )

(add-hook 'nxml-mode-hook #'init-nxml-mode)

;; (define-key nxml-mode-map (kbd "C-c C-i") #'nxml-pretty-format)
(define-key nxml-mode-map (kbd "C-c C-i") #'xml-format-buffer)
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

;; use perl markdown
(setq markdown-command "multimarkdown")

;; (defun init-markdown-mode()
;;   (set (make-local-variable 'company-backends)
;;        '((company-abbrev company-keywords company-ispell)
;;          company-capf company-files))
;;   )

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; use github markdown for readmes
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


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
(ensure-package 'flycheck-plantuml)
(require 'flycheck-plantuml)

(add-to-list 'flycheck-checkers 'plantuml)
;; (flycheck-add-mode 'plantuml 'plantuml-mode)

(setq plantuml-default-exec-mode 'jar)

(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))


(provide 'emacs.common)
;;; emacs.common.el ends here
