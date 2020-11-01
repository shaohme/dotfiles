;;; package --- Summary
;;; Commentary:
;;; common config

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

      )
(when (>= emacs-major-version 27)
  (setq read-process-output-max (* 1024 1024)))
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
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
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


;; change directory immediately to make sure it is enabled
(ispell-change-dictionary "en_US")



;; --- gnus
(require 'mml)

(setq
      ;; 'gnus-w3m seems to disable colouring in mails
      ;; mm-text-html-renderer 'gnus-w3m
      mm-default-directory "~/dwl"
      mm-tmp-directory "~/tmp")






;; --- projectile
;; make emacs project aware
(ensure-package 'projectile)
(require 'projectile)


(setq projectile-completion-system 'ivy ;make ivy aware
      projectile-project-search-path '("~/dev/" "~/work/") ;default paths
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

;; enable globally so that keymap becomes usable from the start
(projectile-mode t)

;; a reddit user suggested this config to make projectile not crawl
;; on remote file systems over TRAMP which can be cumbersome
;; https://www.reddit.com/r/emacs/comments/320cvb/projectile_slows_tramp_mode_to_a_crawl_is_there_a/
(defadvice projectile-project-root (around ignore-remote first activate)
      (unless (file-remote-p default-directory) ad-do-it))


;; --- company
;; basic completion
(ensure-package 'company)
(require 'company)


(setq company-selection-wrap-around t   ;wrap around
      company-minimum-prefix-length 2) ;shorter prefix

(add-hook 'after-init-hook 'global-company-mode)

;; --- flycheck
;; needs to be 'latest' >31 for LSP mode to
;; work properly
(ensure-package 'flycheck)
(require 'flycheck)


;; Wait idle seconds before running flycheck
(setq flycheck-idle-change-delay 2
      ;; jump to next error instead of warning or info
      flycheck-navigation-minimum-level 'error)

(add-hook 'after-init-hook 'global-flycheck-mode)


;;; --- emacs lisp
(setq ;;; have flycheck use emacs load-path when searching
      ;;; for packages
      flycheck-emacs-lisp-load-path load-path
      )

;;; --- xclip
;;; interact with X11 clipboard
(ensure-package 'xclip)
(require 'xclip)

(xclip-mode 1)


;;; Ivy/Counsel/Swiper
(ensure-package 'amx)
(require 'amx)
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

;; as per recommendation from ivy-rich website
(setq ivy-use-virtual-buffers t
      ivy-rich-path-style 'abbrev
      ivy-count-format "(%d/%d) "
      enable-recursive-minibuffers t)
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
(amx-mode 1)

;; replace default keys
(define-key global-map [remap isearch-forward] #'swiper)
(define-key global-map [remap execute-extended-command] #'counsel-M-x)


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


;; --- yasnippet
;; useful to have and recommended by LSP defaults
(ensure-package 'yasnippet)
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


;; lsp-mode
;; LSP compatibility
(ensure-package 'lsp-mode)
(require 'lsp-clients)
(require 'lsp-diagnostics)
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



;; --- yaml mode
(ensure-package 'yaml-mode)
(require 'yaml-mode)

(add-hook 'yaml-mode-hook #'lsp)
(add-hook 'yaml-mode-hook #'indent-tools-minor-mode)




(provide 'emacs.common)
;;; emacs.common.el ends here
