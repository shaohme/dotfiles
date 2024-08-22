;;; package --- init file
;;; Commentary:
;;; Code:

(when (getenv "IS_GENTOO")
  ;; really only optional on gentoo systems
  (require 'site-gentoo nil t))

(defun maybe-load-dir-recursively (site-dir)
  (when (and (file-exists-p site-dir)
             (file-directory-p site-dir))
    (if (not (member site-dir load-path))
        (add-to-list 'load-path site-dir))
    (dolist (file-var (directory-files site-dir t directory-files-no-dot-files-regexp))
	  (when (and (file-directory-p file-var)
		         (not (file-symlink-p file-var))
                 (not (member file-var load-path))
		         (not (string-suffix-p file-var ".el")))
	    (add-to-list 'load-path file-var)))))


;; load local development packages
(maybe-load-dir-recursively (expand-file-name "site-lisp" user-emacs-directory))
(maybe-load-dir-recursively (expand-file-name "~/dev/flymake-yamllint"))
(maybe-load-dir-recursively (expand-file-name "~/dev/monitrc-mode"))
(maybe-load-dir-recursively (expand-file-name "~/dev/fluent-bit-config-mode"))
(maybe-load-dir-recursively (expand-file-name "~/dev/flymake-markdownlint"))
(maybe-load-dir-recursively (expand-file-name "~/dev/flymake-cppcheck"))
(maybe-load-dir-recursively (expand-file-name "~/dev/flymake-plantuml"))
(maybe-load-dir-recursively (expand-file-name "~/dev/flymake-jsonlint"))
(maybe-load-dir-recursively (expand-file-name "~/dev/flymake-pkgcheck"))
(maybe-load-dir-recursively (expand-file-name "~/dev/flymake-xmllint"))
(maybe-load-dir-recursively (expand-file-name "~/dev/ready-made-regexp"))
(maybe-load-dir-recursively (expand-file-name "~/dev/openhab-mode"))
(maybe-load-dir-recursively (expand-file-name "~/dev/yank-indent"))
(maybe-load-dir-recursively (expand-file-name "~/dev/dired-hacks"))


(defvar emacs-host-local (expand-file-name (format "local-%s.el" (system-name)) user-emacs-directory))
(defvar emacs-shared (expand-file-name "shared.el" user-emacs-directory))
(defvar emacs-secrets (expand-file-name "secrets.el" user-emacs-directory))
(defvar emacs-host-custom (expand-file-name (format "custom-%s.el" (system-name)) user-emacs-directory))

;; setup packages
(require 'package)
(load-file (expand-file-name "init-package-archives.el" user-emacs-directory))
(require 'init-package-archives)

(setq package-archive-priorities '(("melpa-stable" . 20)
                                   ("gnu" . 7)
                                   ("nongnu" . 5)
                                   ("melpa" . 2)))

;; set emacs custom file to a host specific custom file
(setq custom-file emacs-host-custom)


(defvar-local package-list nil)
(defvar-local package-refreshed nil)

(setq package-pinned-packages '((protobuf-mode . "melpa")
                                (hcl-mode . "melpa")
                                (ledger-mode . "melpa")
                                (lua-mode . "melpa")
                                (eglot-java . "melpa")
                                (ctags-update . "melpa")
                                (org-download . "melpa")
                                ;; latest poppler 22.xx seems to break
                                ;; build on pdf-tools 0.9x and version
                                ;; 1.0 from nongnu fails to find
                                ;; build-directory
                                (pdf-tools . "melpa")
                                (rg . "melpa")
                                (elfeed . "melpa")
                                (modus-themes . "melpa-stable")
                                (org . "gnu")
                                (tramp . "gnu")
                                (flymake . "gnu")
                                (consult-eglot . "melpa")
                                (caddyfile-mode . "melpa")
                                (org-tree-slide . "melpa")
                                (prettier . "melpa")
                                ;; only exists in melpa for now
                                ;; (clj-refactor . "melpa")
                                ;; stable version incompatible with >27.2
                                (inflections . "melpa")
                                (consult-dir . "melpa")
                                (d-mode . "melpa")
                                ;; seems to be needed for "latest" org
                                ;; mode. 1.3 triggers errors when
                                ;; creating new outlines among others
                                (org-modern . "melpa")
                                (arduino-mode . "melpa")
                                (cargo-mode . "melpa")
                                (clang-format . "melpa")))

(setq package-selected-packages '(gnu-indent tramp orderless vertico diminish ef-themes info-colors which-key mode-line-bell wgrep diredfl marginalia consult flymake project eldoc flymake-proselint notmuch bbdb magit git-modes gitignore-templates languagetool editorconfig rainbow-delimiters highlight-escape-sequences yasnippet eglot slime cider flymake-kondor rust-mode cargo-mode go-mode shfmt lua-mode pip-requirements jq-mode highlight-indentation xml-format auto-rename-tag rainbow-mode typescript-mode markdown-mode markdown-preview-mode dockerfile-mode nginx-mode crontab-mode ssh-config-mode systemd plantuml-mode csv-mode meson-mode cmake-mode cmake-font-lock sqlformat auctex password-store password-store-otp package-lint udev-mode edit-server clj-refactor org ox-hugo org-tree-slide org-superstar ox-reveal syslog-mode pulsar elfeed rg yasnippet-snippets consult-yasnippet kconfig-mode hcl-mode nhexl-mode saveplace-pdf-view i3wm-config-mode protobuf-mode erc html5-schema jsonrpc relint eshell-toggle corfu vundo ledger-mode ascii-table caddyfile-mode nftables-mode standard-themes org-roam org-download pyvenv pyvenv-auto rfc-mode verb djvu modus-themes keycast eros etc-sudoers-mode ellama flymake-ruff python-black reformatter numpydoc consult-dir org-chef org-contrib importmagic go-dlv vcard casual-suite d-mode ada-mode ada-ts-mode ada-ref-man gnuplot snow fireplace arduino-mode yaml-mode embark embark-consult citre haproxy-mode org-modern org-link-beautify expand-region flymake-elisp-config clang-format))

(when (display-graphic-p)
  (add-to-list 'package-selected-packages 'olivetti)
  (add-to-list 'package-selected-packages 'ox-pandoc)
  (add-to-list 'package-selected-packages 'pdf-tools)
  ;; (add-to-list 'package-selected-packages 'spacious-padding)
  )

(when (getenv "IS_VOID")
  (setq source-directory (format "/usr/src/emacs-%s/" emacs-version))
  (add-to-list 'package-selected-packages 'jinx))

;; allow for built in packages to be upgraded
(setq package-install-upgrade-built-in t)

;; perform package installation
(package-install-selected-packages t)
(package-activate-all)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; basic defaults
(setq-default frame-title-format '("%f [" mode-name "] - Emacs"))
(setq initial-major-mode 'text-mode)
;; clean startup screen
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
;; seems disable cursor blinking.
;; used instead of blink-cursor-mode
(setq visible-cursor nil)
;; no backup files
(setq backup-inhibited t)
;; if accidentally enabled, copy files in directory
(setq backup-by-copying t)
;; ease shutdown
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)
;; needed for hungry el packages, like lsp
;; (setq gc-cons-threshold 134217728)
;; always follow symlinks to source controls
(setq vc-follow-symlinks t)
;; scroll one line at a time using keyboard
(setq scroll-step 1)
;; keep cursor at position when scrolling pages. its annoying
;; that it jumps to top or bottom of screen when scrolling pages
(setq scroll-preserve-screen-position t)
;; scroll one line at a time when cursor is positioned outside view.
;; this should make the scrolling experience more smooth instead of
;; `paginated'
(setq scroll-conservatively 101)
;;; keep cursor from hitting window border while scrolling. this can
;;; be annoying when spell correction, navigation, or jumps places the
;;; cursor at the borders where the lack of text around cursor makes
;;; the context harder to see.
(setq scroll-margin 5)
(setq read-process-output-max (* 1024 1024))
;; limit mini buffer size
;; max-mini-window-height 0.10
(setq save-interprogram-paste-before-kill t)
;;; ;;; newline should only be added in file where it matters, like text files
(setq require-final-newline t)
;; if .el is newer than .elc, load .el instead
(setq load-prefer-newer t)
;; no lock files. annoying
(setq create-lockfiles nil)
;; change localization manually for now to make calendar view and
;; timestamps use C wording instead of localized names.
(setq system-time-locale "C")
;;; do not use show visual prompts for actions. keep it in emacs frame.
(setq use-dialog-box nil)
;;; raise threshold. 32MB pdfs opens usually fine enough.
(setq large-file-warning-threshold 67108864)
;;; answer questions easier
;; (defalias 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)
;; start scroll before hitting top or bottom. it can be convenient to
;; see a few lines around the cursor to get a better context when
;; searching
;; (setq-default scroll-margin 5)
;; no need for old versions
(setq delete-old-versions t)
;; disable # files for now. not needed for a long while and often
;; transfered by mistake
(setq auto-save-default nil)
;; Skip confirmation prompts when creating a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)
(setq max-mini-window-height 0.40)
;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;; open URL with default browser instead of EWW
(setq browse-url-generic-program "xdg-open")

(defvar-local my/saves-directory-path (expand-file-name "saves" user-emacs-directory))

(make-directory my/saves-directory-path t)
(setq backup-directory-alist `(("." . ,my/saves-directory-path)))

;; do not need menu key at all
(global-unset-key [menu])


(require 'cus-edit)
;;; show lisp name instead of derived name
(setq custom-unlispify-tag-names nil)


(require 'calendar)
(require 'solar)
;; assume week start at monday
(setq calendar-week-start-day 1)
;; always use 24-hours timestamp
(setq calendar-time-display-form
      '(24-hours ":" minutes
                 (if time-zone " (") time-zone (if time-zone ")")))
;; iso week numbers are not shown by default on calendar view.
;; this is added to display just the week number next to every week
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-function-name-face))



(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))


;; don't kill word, just delete. easier when I just want to delete
(global-set-key [remap backward-kill-word] 'backward-delete-word)


(defvar my/complete-symbol-kbd (kbd "C-M-i") "Default keybind for symbol completion.")
(defvar my/format-kbd (kbd "C-c TAB") "Default keybind for formatting buffers.")
(defvar my/compile-kbd (kbd "C-c C-c") "Default keybind for compiling program.")
;; (defvar my/lsp-code-actions-kbd (kbd "C-c e a") "Default keybind for LSP code actions.")
;; (defvar my/lsp-restart-kbd (kbd "C-c e r") "Default keybind for LSP restart.")
;; (defvar my/help-at-point (kbd "C-h .") "Default keybind for showing help at point.")
(defvar my/comment-kbd (kbd "M-;") "Default keybind for DWIM commenting code.")
(defvar my/other-window-kbd (kbd "M-o") "Default keybind for DWIM commenting code.")
;;; defined as custom to make it visible in `add-dir-local-variable'
(defcustom my/delete-trailing-whitespace t "Instructs whether tailing whitespaces should be deleted on save." :type '(choice (const t) (const nil)))

(require 'window)
;;; windows

(defun my/other-window (count &optional all-frames interactive)
  (interactive "p\ni\np")
  (other-window count all-frames interactive))

(define-key global-map my/other-window-kbd #'my/other-window)
(define-key global-map (kbd "<f9>") #'window-toggle-side-windows)
(define-key global-map (kbd "s-[") #'(lambda () (interactive)(shrink-window 2)))
(define-key global-map (kbd "s-]") #'(lambda () (interactive)(enlarge-window 2)))

;; make text in/de-creasing easier to remember
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") #'(lambda () (interactive) (text-scale-set 0)))

(define-key global-map (kbd "C-x !") #'delete-other-windows-vertically)

(require 'windmove)

;; move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control)

;;; wrap around to emulate i3 behaviour
(setq windmove-wrap-around t)

(define-key global-map (kbd "C-s-h") #'windmove-swap-states-left)
(define-key global-map (kbd "C-s-l") #'windmove-swap-states-right)
(define-key global-map (kbd "C-s-k") #'windmove-swap-states-up)
(define-key global-map (kbd "C-s-j") #'windmove-swap-states-down)


(when (display-graphic-p)
  (pixel-scroll-precision-mode)
  ;; Make right-click do something sensible
  (context-menu-mode))

;; fix archaic defaults
(setq sentence-end-double-space nil)

;; make switching buffers more consistent
(setq switch-to-buffer-obey-display-actions t)



(defun maybe-delete-trailing-whitespace ()
  (when (and (not (derived-mode-p 'fundamental-mode))
             my/delete-trailing-whitespace)
    (delete-trailing-whitespace)))

;; might as well delete trailing whitespace. strictly speaking not
;; needed if the source formatting tool supports whitespace removal.
(add-hook 'before-save-hook #'maybe-delete-trailing-whitespace)

;;; --- so-long
;;; this mode steps in and disable the intended major mode
;;; if a file with long lines is detected, like minified JSON/JS etc.
;;; many major modes struggles with these kinds of files.
(require 'so-long)

(global-so-long-mode 1)

;;; --- makefile
(require 'make-mode)

(define-key makefile-mode-map my/comment-kbd #'comment-dwim)
(define-key makefile-mode-map (kbd "C-M-i") #'completion-at-point)

(add-to-list 'auto-mode-alist '("Makefile.*" . makefile-mode))


(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))


;; --- tramp
(require 'tramp)
(require 'tramp-sh)

(setq tramp-default-method "ssh")
(setq tramp-backup-directory-alist backup-directory-alist)
;; WARNING: might be a security issue
(setq tramp-allow-unsafe-temporary-files t)

;;; try speed up tramp when VC is enabled
(setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                   vc-ignore-dir-regexp
                                   tramp-file-name-regexp))

(defvar my/tramp-auto-save-directory (expand-file-name "tramp-auto-save" user-emacs-directory))
(make-directory my/tramp-auto-save-directory t)

(setq tramp-auto-save-directory my/tramp-auto-save-directory)

;;; make TRAMP reuse ssh controlmaster
(setq tramp-ssh-controlmaster-options (concat "-o ControlPath=~/.ssh/master-%%r-at-%%h-%%p "
                                              "-o ControlMaster=auto -o ControlPersist=yes"))
(setq tramp-use-connection-share t)

(setq tramp-connection-timeout 5)

(connection-local-set-profile-variables
 'guix-system
 '((tramp-remote-path . (tramp-own-remote-path))))
(connection-local-set-profiles
 '(:application tramp :protocol "sudo" :machine "rw")
 'guix-system)


;; --- recentf
(require 'recentf)
;; limit recentf
(setq recentf-max-saved-items 1000)
;; not too many recentf menu items
(setq recentf-max-menu-items 25)
;; exclude unneeded
(setq recentf-exclude '("^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'"
                        ".*-autoloads\\.el\\'"
                        "[/\\]\\.elpa/"
                        "/tmp/"
                        "/su:"
                        "/sudo:"
                        "/ssh:"
                        (concat package-user-dir "/.*-autoloads\\.el\\'")))

;; have list of recent files
(recentf-mode t)


(require 'help)
(require 'shortdoc)
(require 'man)


;; -- comint
(require 'comint)
(require 'compile)

(setq comint-scroll-to-bottom-on-output 'this)
(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error t)

;; do not ask about saving buffers before compiling/building/etc.
;; this will save all unsaved buffers though.
(setq compilation-ask-about-save nil)

;;; programs like make/cmake sometimes produces ansi escape sequences,
;;; garbling the output.
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; --- spelling
(require 'ispell)

;; must be set before loading flyspell to not replace C-M-i completion
;; on load
(setq-default flyspell-use-meta-tab nil)

(require 'flyspell)


(setq flyspell-prog-text-faces
      (delq 'font-lock-string-face flyspell-prog-text-faces))

;; default to en-us dicts and choose hunspell
(setq ispell-program-name (executable-find "hunspell"))
(setq ispell-dictionary "british")
(setq ispell-really-hunspell t)
(setq ispell-personal-dictionary (expand-file-name "~/.ispell"))
(setq flyspell-mode-line-string " FS")

;; change directory immediately to make sure it is enabled
(ispell-change-dictionary "british")


;; --- dictionary
(require 'dictionary)

  ;;; prevent buffer litter
(setq dictionary-use-single-buffer t)
(setq dictionary-server "localhost")
(setq dictionary-port 2628)
(setq dictionary-default-dictionary "!")

(defun my/help-at-pt (&optional arg)
  (interactive "P")
  (let ((res (display-local-help arg)))
    (if (string-match "No local help at point" res)
        (dictionary-search (thing-at-point 'word 'no-properties)))))


;; --- book marks
(require 'bookmark)
(require 'casual-bookmarks)
;; save bookmarks to file after 1 is added
(setq bookmark-save-flag 1)

(define-key bookmark-bmenu-mode-map (kbd "C-o") #'casual-bookmarks-tmenu)

(add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)


(defun my/bookmark-set ()
  "Overwrite a given bookmark.
Use `completing-read' to make it easier overwriting existing
bookmarks. Like when updating the same bookmark multiple times
during reading."
  (interactive)
  (let ((chosen (completing-read "Set bookmark (overwrite): " (bookmark-all-names))))
    (bookmark-set-internal nil chosen 'overwrite)))


(define-key global-map [remap bookmark-set] 'my/bookmark-set)


;; --- hippie-expand
(require 'hippie-exp)
(require 'dabbrev)

;; replace dabbrev-expand with hippie to have more expanding features
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill))
(setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))


;; --- auto revert
(require 'autorevert)

(setq auto-revert-verbose t)
(setq auto-revert-stop-on-user-input nil)
(setq revert-without-query (list "."))
;;; auto revert non-file buffers, like Dired.
;;; this could cause a performance hit
;; (setq global-auto-revert-non-file-buffers t)

(global-auto-revert-mode t)



;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)

;; --- cosmetics
(require 'diminish)


(require 'modus-themes)


;; custom theme loading functions to customize colors not otherwise
;; touched by theme
(defun load-light-theme()
  (interactive)
  (load-theme 'ef-light t nil))

(defun load-dark-theme()
  (interactive)
  (load-theme 'ef-dark t nil))

;; load theme based on whether theme file is set to light or dark
(let ((f (substitute-in-file-name (expand-file-name "~/.theme"))))
  (if (file-exists-p f)
	  (if (with-temp-buffer
			(insert-file-contents (substitute-in-file-name f))
			(goto-char (point-min))
			(looking-at "0"))
          (load-dark-theme)
        (load-light-theme))
    (load-light-theme)))

(blink-cursor-mode 0)
;; show line numbers
(line-number-mode t)
;; show column number
(column-number-mode t)
;; remove menu bar. must be -1 to removex
(unless (eq window-system 'ns)
  ;; try mitigate problem with cut-off text/content at bottom/top when
  ;; emacs is resized under some windows managers
  (setq frame-resize-pixelwise t)
  (setq window-resize-pixelwise t))


;; show arrows and markes on margin to indicate buffer length
(setq indicate-buffer-boundaries 'left)



;; --- visual-line

(require 'simple)

;;; add indicators for newlines on both sides
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(diminish 'visual-line-mode)



;; --- winner
;; mostly for undo window changes when accidentally hitting C-x [01]
(require 'winner)

(setq winner-boring-buffers '("*Completions*"
                              "*Compile-Log*"
                              "*inferior-lisp*"
                              "*Fuzzy Completions*"
                              "*Apropos*"
                              "*Help*"
                              "*cvs*"
                              "*Buffer List*"
                              "*Ibuffer*"
                              "*esh command on file*"))

(diminish 'winner-mode)

(winner-mode t)

(define-key winner-mode-map (kbd "<f11>") 'winner-undo)
(define-key winner-mode-map (kbd "<f12>") 'winner-redo)


;; name buffers nicer names if identical
(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(add-hook 'shell-mode-hook #'rename-uniquely)


;;; --- info mode

;; add more colors to info mode
(require 'info)
(require 'info-colors)
(require 'casual-info)

(add-hook 'Info-selection-hook #'info-colors-fontify-node)

(define-key Info-mode-map (kbd "C-o") #'casual-info-tmenu)

(require 'image-mode)

(defun my/image-mode-print-size()
  (interactive)
  (message "Size: %s" (image-display-size (image-get-display-property) t)))

(defun my/first-image-position()
  ;; some buffers showing image like plantuml preview buffers have
  ;; cursors located elsewhere than the rendered image. this makes
  ;; some image manipulation commands like `image-increase-size' not
  ;; work unless the cursor is moved to the image position. this
  ;; function tries to guess the image position.
  ;;
  ;; FIXME: quick
  ;; hack to get image at cursor for image mode. `point' have been
  ;; observed to be at values like 7685
  (goto-char 1))

(defun my/image-mode-quit()
  (interactive)
  (quit-window t nil))

(defun my/image-increase-size()
  (interactive)
  (let ((pos (my/first-image-position)))
    (image-increase-size nil pos)))

(defun my/image-decrease-size()
  (interactive)
  (let ((pos (my/first-image-position)))
    (image-decrease-size nil pos)))

;; workout bugs, and get rid of the default "i" prefix.
(define-key image-mode-map (kbd "p") #'my/image-mode-print-size)
(define-key image-mode-map (kbd "+") #'my/image-increase-size)
(define-key image-mode-map (kbd "-") #'my/image-decrease-size)
(define-key image-mode-map (kbd "q") #'my/image-mode-quit)


(require 'pulsar nil t)
(when (display-graphic-p)
  (setq pulsar-pulse-functions
        ;; NOTE 2022-04-09: The commented out functions are from before
        ;; the introduction of `pulsar-pulse-on-window-change'.  Try that
        ;; instead.
        '(recenter-top-bottom
          move-to-window-line-top-bottom
          reposition-window
          bookmark-jump
          my/other-window
          other-window
          delete-window
          delete-other-windows
          forward-page
          backward-page
          scroll-up-command
          scroll-down-command
          next-buffer
          previous-buffer
          windmove-right
          windmove-left
          windmove-up
          windmove-down
          windmove-swap-states-right
          windmove-swap-states-left
          windmove-swap-states-up
          windmove-swap-states-down
          tab-new
          tab-close
          tab-next
          org-next-visible-heading
          org-previous-visible-heading
          org-forward-heading-same-level
          org-backward-heading-same-level
          outline-backward-same-level
          outline-forward-same-level
          outline-next-visible-heading
          outline-previous-visible-heading
          outline-up-heading))

  ;; (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)

  (pulsar-global-mode 1)

  ;; integration with the built-in `imenu':
  (add-hook 'imenu-after-jump-hook #'pulsar-recenter-middle)
  (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry))



(require 'nhexl-mode)


;; --- ibuffer
(require 'ibuffer)
(require 'casual-ibuffer)
;;; WARNING: `ibuffer-project' seems to hurt benchmark, using vc packages
;; (require 'ibuffer-project)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (file-size-human-readable (buffer-size)))


(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; otherwise its bound to `ibuffer-visit-buffer-1-window'
(define-key ibuffer-mode-map (kbd "M-o") #'my/other-window)
(define-key ibuffer-mode-map (kbd "C-o") #'casual-ibuffer-tmenu)


(require 'vcard)


;; --- which-key mode
;; show keybindings in popup when typing
(require 'which-key)

;; remove lighter from modeline
(setq which-key-lighter "")

(which-key-mode t)

;;; doesnt show keymap for current mode. hence commented out.
;; (global-set-key (kbd "C-h W") 'which-key-C-h-dispatch)


;; --- mode line bell
;; instead of sound bell. should work in terminal as well
(require 'mode-line-bell)

(mode-line-bell-mode t)


(add-to-list 'auto-mode-alist '("\\mimeapps.list\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("pycodestyle" . conf-mode))
(add-to-list 'auto-mode-alist '(".*/waybar/config.*" . json-mode))

;; --- project
(require 'project)

(setq project-vc-ignores (list
                          ;; from ccls/clang
                          ".cache/"))

(add-to-list 'project-switch-commands '(rg-project "rg-project" ?r) t)

(setq project-vc-extra-root-markers '(".project"))



;; --- grep'ing
;; tried rg.el but it had many user questions and could not
;; open remote file over TRAMP automatically from the result list
(require 'wgrep)

(require 'rg)

(rg-enable-default-bindings)


(setq-default grep-highlight-matches t
              grep-scroll-output t)
;; hit w to enter editor mode
(dolist (key (list (kbd "C-c C-q") (kbd "w")))
  (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode))


;; --- saveplace
(require 'saveplace)
;; remember cursor locations
(require 'saveplace-pdf-view)

;; dont check file is readable. can slow quit
;; if file is on remote filesystems
(setq save-place-forget-unreadable-files nil)

;; remember file positions
(save-place-mode 1)


;; --- savehist
;; add minibuffer history
(require 'savehist)

(setq history-length 10000)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(savehist-mode 1)



;; --- dired
;; more colors in dired
(require 'diredfl)
(require 'dired-x)
(require 'dired-open nil t)
(require 'image-dired)
(require 'casual-dired)
;; (require 'dired-preview)

;; try out. sometimes previewed pictures can be convenient
;; (dired-preview-global-mode 1)

(define-key dired-mode-map (kbd "C-o") 'casual-dired-tmenu)

;;; makes `dired-single' obsolete
;;;
;;; working with files across buffer seems already requiring you to
;;; have all dired trees in same buffer using `i'
(setq dired-kill-when-opening-new-dired-buffer t)

;;; make dired more intuitive by making it guess your action based on
;;; current open windows/buffers
(setq dired-dwim-target t)


(setq image-dired-external-viewer "eog")

(diredfl-global-mode t)


;; --- diffing and merging
(require 'ediff)
(require 'ediff-merg)
(require 'diff-mode)
(require 'smerge-mode)
;; NOTE: ediff has superseded emerge, so emerge should not be used

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


(defun my/ediff-quit-manually (reverse-default-keep-variants)
  "Finish an Ediff session and exit Ediff.
Unselects the selected difference, if any, restores the read-only and modified
flags of the compared file buffers, kills Ediff buffers for this session
\(but not buffers A, B, C).

If `ediff-keep-variants' is nil, the user will be asked whether the buffers
containing the variants should be removed \(if they haven't been modified).
If it is t, they will be preserved unconditionally.  A prefix argument,
temporarily reverses the meaning of this variable."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (let ((ctl-buf (current-buffer))
	    (ctl-frm (selected-frame))
	    (minibuffer-auto-raise t))
	(progn
	  (message "")
	  (setq this-command 'ediff-quit)   ; bug#38219
	  (set-buffer ctl-buf)
	  (ediff-really-quit reverse-default-keep-variants))))

(defun my/ediff-startup-hook-setup ()
  ;; move to the first difference
  (ediff-next-difference)
  ;; move to the merged buffer window
  ;; seems not to be defined
  ;; (winum-select-window-by-number 3)
  ;; save the windows layout
  (window-configuration-to-register ?a)
  (local-set-key (kbd "q") #'my/ediff-quit-manually))

(add-hook 'ediff-startup-hook #'my/ediff-startup-hook-setup)

;; ediff does not seem to combine both A and B buffers into C when
;; typing '+' . found this fix to manually combine the two diff
;; instead and copy to C without <<<<<<<<< and >>>>>>>> headers too
(defun my/ediff-copy-both-to-c ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun my/add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'my/ediff-copy-both-to-c))

(add-hook 'ediff-keymap-setup-hook 'my/add-d-to-ediff-mode-map)

(defun my/ediff-quit ()
  ;; killing windows sometimes break, ex. in diffs
  ;; (ediff-kill-buffer-carefully ediff-window-A)
  ;; (ediff-kill-buffer-carefully ediff-window-B)
  ;; (ediff-kill-buffer-carefully ediff-window-C)
  (ediff-kill-buffer-carefully ediff-buffer-A)
  (ediff-kill-buffer-carefully ediff-buffer-B)
  (ediff-kill-buffer-carefully ediff-buffer-C)
  (ediff-kill-buffer-carefully ediff-control-frame)
  (ediff-kill-buffer-carefully "*Ediff Control Panel*"))

;; try cleanup after ediff windows
(add-hook 'ediff-quit-hook #'my/ediff-quit)

;; removed diff-goto-source from default keymap.
;; interferes with buffer navigation key
(define-key diff-mode-map (kbd "M-o") nil)


;; --- regex builder
(require 're-builder)
(require 'casual-re-builder)
;; Support a slightly more idiomatic quit binding in re-builder
(define-key reb-mode-map (kbd "C-c C-k") 'reb-quit)

(define-key reb-mode-map (kbd "C-o") #'casual-re-builder-tmenu)

(setq reb-re-syntax 'string)



;; --- eldoc
(setq-default eldoc-minor-mode-string nil)

(require 'eldoc)

;; do not expand minibuffer when docstring are longer
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)

(global-eldoc-mode 1)



;; --- isearch, grep and replace
(require 'isearch)

(setq isearch-lazy-count t)
(setq isearch-allow-scroll t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format " (%s/%s)")

(require 'xref)

(setq xref-search-program 'ripgrep)

(require 'replace)

(add-hook 'occur-mode-hook #'hl-line-mode)

(require 'warnings)


;; --- tree-sitter
(require 'treesit)

;; ignore warning from missing grammas for now.
(add-to-list 'warning-suppress-log-types '(treesit))
(add-to-list 'warning-suppress-types '(treesit))



;; --- completion and minibuffer
(require 'vertico)
(require 'vertico-multiform)
(require 'orderless)
(require 'marginalia)

(setq vertico-count 25)
(setq vertico-resize 'grow-only)
(setq vertico-cycle t)

(vertico-mode t)
(vertico-multiform-mode t)

;;; some completions are shown better in a buffer than in minibuffer
(setq vertico-multiform-commands
      '((consult-imenu buffer)
        (consult-find buffer)))

(setq vertico-multiform-categories
      '((consult-grep buffer)))

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
;; (when (> emacs-major-version 27)
;;   )
(setq read-extended-command-predicate #'command-completion-default-include-p)


;;; --- corfu

(require 'corfu)
(require 'corfu-history)
(require 'corfu-popupinfo)

;;; whitespace charater used for insertion. especially useful when
;;; using orderless completion.
(setq corfu-separator ?\s)
(setq corfu-auto nil)
(setq corfu-count 10)
(setq corfu-cycle t)
(setq corfu-popupinfo-delay '(1.0 . 0.5))

(add-hook 'corfu-mode-hook #'corfu-history-mode)

(add-hook 'prog-mode-hook #'corfu-mode)
(add-hook 'shell-mode-hook #'corfu-mode)
(add-hook 'eshell-mode-hook #'corfu-mode)

;; insert SPC separator as set by `corfu-separator' to indicate
;; completion is not done when using orderless. completion finished
;; when candidate is chosen or C-g.
(define-key corfu-mode-map (kbd "SPC") #'corfu-insert-separator)

(corfu-popupinfo-mode 1)


;;; --- activities

;; (require 'activities)
;; (require 'activities-list)
;; (require 'edebug)

;; (activities-mode)

;; (setq edebug-inhibit-emacs-lisp-mode-bindings t)

;; (define-key global-map (kbd "C-x C-y C-n") #'activities-new)
;; (define-key global-map (kbd "C-x C-y C-d") #'activities-define)
;; (define-key global-map (kbd "C-x C-y C-a") #'activities-resume)
;; (define-key global-map (kbd "C-x C-y C-s") #'activities-suspend)
;; (define-key global-map (kbd "C-x C-y C-k") #'activities-kill)
;; (define-key global-map (kbd "C-x C-y RET") #'activities-switch)
;; (define-key global-map (kbd "C-x C-y b") #'activities-switch-buffer)
;; (define-key global-map (kbd "C-x C-y g") #'activities-revert)
;; (define-key global-map (kbd "C-x C-y l") #'activities-list)


;;; --- consult

(require 'consult)
(require 'consult-xref)
(require 'consult-flymake)
(require 'consult-imenu)
(require 'consult-register)
(require 'consult-kmacro)
(require 'consult-dir)
(require 'consult-org)
(require 'consult-info)

(setq xref-show-xrefs-function #'consult-xref)
(setq xref-show-definitions-function #'consult-xref)

;; integration with the `consult' package:
(add-hook 'consult-after-jump-hook #'pulsar-recenter-middle)
(add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)


(define-key global-map (kbd "M-g g") #'consult-goto-line)
(define-key global-map (kbd "M-g o") #'consult-org-agenda)
(define-key global-map (kbd "M-s r") #'consult-ripgrep)
(define-key global-map (kbd "M-s s") #'consult-outline)
(define-key global-map (kbd "M-s m") #'consult-mark)
(define-key global-map (kbd "M-s i") #'consult-info)
(define-key global-map (kbd "C-x b") #'consult-buffer)
(define-key global-map (kbd "C-c h") #'consult-history)
(define-key global-map (kbd "C-c m") #'consult-mode-command)
(define-key global-map (kbd "C-c k") #'consult-kmacro)
(define-key global-map (kbd "C-x M-:") #'consult-complex-command)
(define-key global-map (kbd "C-x 4 b") #'consult-buffer-other-window)
(define-key global-map (kbd "C-x 5 b") #'consult-buffer-other-frame)
(define-key global-map (kbd "C-x r b") #'consult-bookmark)
(define-key global-map (kbd "C-x p b") #'consult-project-buffer)
(define-key global-map (kbd "C-x r j") #'consult-register)
(define-key global-map (kbd "M-y") #'consult-yank-pop)

;; not using existing (tab-to-tab-stop) so binding it to imenu instead
(define-key global-map (kbd "M-i") #'consult-imenu)

(define-key global-map (kbd "C-x C-d") #'consult-dir)
(define-key minibuffer-local-completion-map (kbd "C-x C-d") #'consult-dir)
(define-key minibuffer-local-completion-map (kbd "C-x C-j") #'consult-dir-jump-file)

;; `basic' is added to as fallback
(setq completion-styles '(orderless basic))
;; (setq completion-category-defaults nil)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

(orderless-define-completion-style orderless+initialism (orderless-matching-styles '(orderless-initialism
                                                                                     orderless-literal
                                                                                     orderless-regexp)))

(orderless-define-completion-style orderless+prefix (orderless-matching-styles '(orderless-prefixes
                                                                                 orderless-literal)))

;; `file' is still `basic', though not needed, because its more
;; intuitive to select files from file prefix than complete orderless
(setq completion-category-overrides '((command (styles orderless+initialism))
                                      (symbol (styles orderless+initialism))
                                      (file (styles basic orderless+initialism))
                                      (variable (styles orderless+initialism))))

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function (lambda (&rest args)
				                      (apply (if vertico-mode
						                         #'consult-completion-in-region
					                           #'completion--in-region)
					                         args)))

(defun my/custom-root-directory (&optional maybe-ask)
  "Attempts to determine the project root directory using difference techniques.
 It favors source control direcotries."
  (or (vc-root-dir)
      (locate-dominating-file "." ".git")
      (locate-dominating-file "." ".project")
      default-directory))

(setq consult-project-function #'my/custom-root-directory)

(marginalia-mode t)

(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)



;; --- embark
(require 'embark)
(require 'embark-consult)

(define-key global-map (kbd "C-.") #'embark-act)
;; not set because `embark-dwim' shows flymake-diagnostics buffer when
;; M-. on a line with flymake error.
;;
;; (define-key global-map (kbd "M-.") #'embark-dwim)


;; --- flymake
(require 'flymake)
(require 'flymake-proselint)

(defun init-tabulated-buffer()
  "TODO: Ugly hack to sort flymake buffer by severity."
  (when (derived-mode-p 'flymake-diagnostics-buffer-mode)
    ;; index 2 is expected to have "Type" column.
    (tabulated-list-sort 2)
    ;; invoked twice because first sort is descending, leaving errors
    ;; and warning at the bottom
    (tabulated-list-sort 2)))

(add-hook 'tabulated-list-mode-hook #'init-tabulated-buffer)

;; some linters checks file and not from stdin, so check-as-you-type
;; would produce wrong line-col numbers until the file gets saved
;; again backend
(setq flymake-no-changes-timeout 0.5)
(setq flymake-start-on-flymake-mode t)
(setq flymake-start-on-save-buffer t)
(setq flymake-mode-line-format '(" FM" flymake-mode-line-exception flymake-mode-line-counters))
(setq flymake-mode-line-counter-format '(" [" flymake-mode-line-error-counter
                                         flymake-mode-line-warning-counter
                                         flymake-mode-line-note-counter "]"))


(define-key flymake-mode-map (kbd "C-c ! s") #'flymake-start)
(define-key flymake-mode-map (kbd "C-c ! d") #'flymake-show-buffer-diagnostics)
(define-key flymake-mode-map (kbd "C-c ! n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c ! p") #'flymake-goto-prev-error)



;; --- language tool
(require 'languagetool)

(setq languagetool-mother-tongue "da-DK")

(setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"))
(setq languagetool-console-command (expand-file-name "/usr/lib/languagetool/languagetool-commandline.jar"))
(setq languagetool-server-command nil)
(setq languagetool-java-arguments (list "-Dfile.encoding=UTF-8" "-Xmx12G" "-Xms256m"))
(setq languagetool-server-url "http://localhost")
(setq languagetool-server-port 8192)


;; used for count-page-lines which I don't need
(global-unset-key (kbd "C-x l"))

(global-set-key (kbd "C-x l c") 'languagetool-check)
(global-set-key (kbd "C-x l d") 'languagetool-clear-suggestions)
(global-set-key (kbd "C-x l .") 'languagetool-correct-at-point)
(global-set-key (kbd "C-x l b") 'languagetool-correct-buffer)
(global-set-key (kbd "C-x l l") 'languagetool-set-language)


;; --- org-mode
;; because, of course
(require 'org)
(require 'org-contrib)
(require 'org-refile)
(require 'org-protocol)
(require 'org-capture)
(require 'org-agenda)
(require 'ox-icalendar)
(require 'org-id)
(require 'find-lisp)
(require 'ox-hugo)
(when (executable-find "pandoc")
  (require 'ox-pandoc nil t))
(require 'ob-plantuml)
(require 'org-tree-slide)
(require 'org-superstar)
(require 'org-download)
(require 'ox-reveal)
;; to remove clutter when viewing org files with lots of PROPERTIES
;; (require 'org-tidy)
(require 'org-chef)

;; usually prefer to see two weeks overview
(setq org-agenda-span 'fortnight)

(load-file (expand-file-name "init-org.el" user-emacs-directory))
(require 'init-org)

(load-file (expand-file-name "org-shared.el" user-emacs-directory))
(require 'org-shared)


(setq org-agenda-default-appointment-duration 60)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))
;; have `org-todo' show up in minibuffer instead of a separate buffer
;; and windows. if not in minibuffer org-todo will rearrange windows
;; until todo state is chosen, which is annoying.
(setq org-use-fast-todo-selection 'expert)
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
;; show full link or not. kept compact because urls are often quite long.
(setq org-link-descriptive t)
(setq org-id-link-to-org-use-id t)
(setq org-agenda-text-search-extra-files nil)
;; its convenient to list more headings in imenu. so far no
;; performance issues.
(setq org-imenu-depth 9)

;; force new page after toc
(setq org-latex-toc-command "\\tableofcontents \\clearpage")

;;; allow for pasting dragged links and pastes anywhere in the file
(setq org-download-heading-lvl nil)

;;; during work we most likely also need a Word file
(setq org-odt-preferred-output-format "doc")

(if (file-directory-p (expand-file-name org-directory))
    (mapc (lambda (elt) (add-to-list 'org-agenda-text-search-extra-files elt))
          (find-lisp-find-files (expand-file-name org-directory) "\\.org")))


(setq org-file-apps '((auto-mode . emacs)
                      (directory . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . "librewolf %s")
                      ("\\.pdf\\'" . default)))

;; set timestamp when finishing
(setq org-log-done 'time)
;; %? = cursor location
(setq org-capture-templates '(("i" "Inbox" entry (file "inbox.org")
                               "* TODO %?\n")
                              ("e" "Events" entry (file+headline "events.org" "Events")
                               "* %?\n")
                              ("m" "Meeting" entry (file+headline "meetings.org" "Future")
                               "* %? :meeting:\n<%<%Y-%m-%d %a %H:00>>")
                              ("n" "Note" entry  (file "notes.org")
                               "* Note (%a)\n\n%?")
                              ;; beware changing `key' as its used with bookmarks link in Firefox
                              ("w" "Temp Links from the interwebs" item
                               (file+headline "links.org" "Temporary Links")
                               "%?\n \%i\n %a")
                              ("j" "Journal" entry (file+datetree "journal.org")
                               "* %?\n  %i\n  %a")
                              ("c" "Cookbook" entry (file "cookbook.org")
                               "%(org-chef-get-recipe-from-url)"
                               :empty-lines 1)
                              ("C" "Manual Cookbook" entry (file "cookbook.org")
                               "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")
                              ("@" "Inbox [mu4e]" entry (file "inbox.org")
                               "* TODO Reply to \"%a\" %?\n")))


(defun my/org-export-plantuml (current-backend)
  "Try export all plantuml files before exporting org files."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (parent-dir (file-name-directory (directory-file-name file-name))))
    (when (and (equal current-backend 'html)
               parent-dir)
      (message "Begin exporting plantuml in dir %s" parent-dir)
      (shell-command (format "cd %s && plantuml" parent-dir))
      (message "Done exporting plantuml in dir %s" parent-dir))))

(when (> emacs-major-version 28)
  (add-to-list 'org-export-before-parsing-functions #'my/org-export-plantuml))

(defun make-capture-frame (&optional capture-url)
  "Create a new frame and run org-capture."
  (interactive)
  (org-protocol-capture capture-url))


;; --- org-agenda

;; `%b' is appended to have breadcrumbs on TODOs. it helps with
;; context when reading a large list.
(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 (todo . " %i %-12:c %b")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

(setq org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                    (todo priority-down todo-state-down category-keep)
                                    (tags priority-down category-keep)
                                    (search category-keep)))

(defun my/save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation."
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
                         (seq-some (lambda (x) (string-match x (buffer-file-name))) org-agenda-files)))
  (message "Saving org-agenda-files buffers... done"))

;; org buffers should save after refile to have changes available to
;; other programs quickly.
(advice-add 'org-refile :after (lambda (&rest _)
                                 (my/save-org-buffers)))


(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)


(defun my/org-agenda-quit()
  "Save org buffers before agenda buries."
  (interactive)
  (org-save-all-org-buffers)
  (org-agenda-quit))

(define-key org-mode-map (kbd "M-.") 'org-open-at-point)
;; have agenda save buffers after quit to make sure buffers are saved
;; quickly for other programs to use.
(define-key org-agenda-mode-map [remap org-agenda-quit] #'my/org-agenda-quit)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(define-key org-mode-map (kbd "C-c C-r") verb-command-map)

(defun org-search-args ()
  "Search org directory using consult-ripgrep. With live-preview."
  (let ((consult-ripgrep-args "rg --null --ignore-case --type org --line-buffered --color=never --max-columns=500 --no-heading --line-number"))
    (consult-ripgrep org-directory)))

(defun headlong ()
  "Make the current minibuffer completion exit when there is 1 candidate."
  (add-hook 'after-change-functions
            (lambda (&rest _)
              (let* ((all (completion-all-completions
                           (minibuffer-contents)
                           minibuffer-completion-table
                           minibuffer-completion-predicate
                           (max 0 (- (point) (minibuffer-prompt-end)))))
                     (last (last all)))
                (when last (setcdr last nil))
                (when (and all (null (cdr all)))
                  (delete-minibuffer-contents)
                  (insert (car all))
                  (exit-minibuffer))))
            nil t))

(defun org-search ()
  (interactive)
  (minibuffer-with-setup-hook #'headlong (funcall #'org-search-args)))

(global-set-key (kbd "C-c o s") 'org-search)

(defun zorg-backlinks ()
  "Search for backlinks to current entry."
  (interactive)
  (let ((link (condition-case nil
                  (org-id-store-link)
                (error "Unable to create a link to here"))))
    (org-occur-in-agenda-files (regexp-quote link))))


(setq org-reveal-root (expand-file-name "revealjs" user-emacs-directory))
(setq org-reveal-single-file t)

(setq org-plantuml-jar-path (expand-file-name "~/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(defun my/org-confirm-babel-evaluate (lang body)
  (not (string= lang "plantuml")))

(setq org-confirm-babel-evaluate #'my/org-confirm-babel-evaluate)

;;; seems relevant for now to jump more easily.
(define-key org-mode-map (kbd "M-n") #'org-next-visible-heading)
(define-key org-mode-map (kbd "M-p") #'org-previous-visible-heading)


(defun init-org-mode ()
  ;; to mitigate M-j error: default-indent-new-line: Wrong type
  ;; argument: char-or-string-p, nil
  (setq fill-prefix "")
  (push "WHITESPACE_RULE" languagetool-local-disabled-rules))

(add-hook 'org-mode-hook #'init-org-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'corfu-mode)
(add-hook 'org-mode-hook #'electric-pair-local-mode)
;; (add-hook 'org-mode-hook #'org-tidy-mode)
;; source block can be huge
(add-hook 'org-mode-hook #'org-fold-hide-block-all)

(when (display-graphic-p)
  (add-hook 'org-mode-hook #'org-download-enable))


(setq org-id-extra-files 'org-agenda-text-search-extra-files)

;; opens agenda files in buffers. if this happens 'too early' during
;; startup not all modes are loaded with them and have to be manually
;; reloaded again.
;;
;; (org-id-update-id-locations)

(defun org-id-complete-link ()
  "Create an id: link using completion."
  (concat "id:" (org-id-get-with-outline-path-completion org-refile-targets)))

(org-link-set-parameters "id" :complete #'org-id-complete-link)



;;; --- org beautify
;; (require 'org-bullets)
;;; hide italic and bold markers
;; (setq org-hide-emphasis-markers t)
;;; needed for `org-link-beautify' to work
;; (require 'org-element-ast)
(require 'org-modern)
;; (require 'org-link-beautify)

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
;; (add-hook 'org-mode-hook #'org-bullets-mode)
;; (add-hook 'org-mode-hook #'org-link-beautify-mode)

;; --- bbdb
(require 'bbdb)

(setq bbdb-mua-pop-up t)
(setq bbdb-mua-pop-up-window-size 5)


(require 'jinx nil t)
(when (fboundp 'jinx-mode)
  (setq jinx-languages "en_GB")
  ;; if 'ask jinx will ask if
  (setq jinx-save-languages nil)
  (global-set-key [remap ispell-word] #'jinx-correct)
  (global-set-key (kbd "C-M-$") #'jinx-languages)

  ;; want pink for spelling errors
  (set-face-underline 'jinx-misspelled "#f677e9")

  ;; (diminish 'jinx-mode)

  (global-jinx-mode 1))


;;; WARNING: use names from `ispell-dicts-name2locale-equivs-alist' to
;;; avoid .aff/.dic file names differences on hunspell package
;;; distributions.
(defun switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "british") "dansk" "british")))
    (ispell-change-dictionary change)
    (if (featurep 'languagetool)
        (languagetool-set-language (replace-regexp-in-string "\_" "-" change)))
    ;; Dont check whole buffer as it can be of several languages, and large
    (if (bound-and-true-p flyspell-mode)
        (flyspell-buffer))
    (when (bound-and-true-p jinx-mode)
      (jinx-languages (cond ((string= change "dansk") "da_DK")
                            ((string= change "british") "en_GB"))))

    (message "Dictionary switched from %s to -> \" %s \"" dic change)))


;;; easier dictionary switching
(global-set-key (kbd "<f8>") 'switch-dictionary)


;; --- gnus, notmuch, mailing and messaging
(require 'gnus-agent)
(require 'gnus-cache)
(require 'gnus-icalendar)
(require 'nndraft)
(require 'nnfolder)
(require 'smtpmail)
(require 'sendmail)


;; gnus new home
(setq gnus-directory (expand-file-name "gnus" user-emacs-directory))
;; do not read or write to .newsrc. we dont plan to use other
;; reader along with gnus
(setq gnus-read-newsrc-file nil)
(setq gnus-save-newsrc-file nil)
;; set new cache directories
(setq gnus-cache-directory (file-name-as-directory (expand-file-name "cache" gnus-directory)))
;; no slash! this is a file, not a directory!
(setq gnus-cache-active-file (expand-file-name "active" (expand-file-name "cache" gnus-directory)))
;; move gnus related files into its dir
(setq gnus-article-save-directory (file-name-as-directory (expand-file-name "save" gnus-directory)))
(setq gnus-kill-files-directory (file-name-as-directory (expand-file-name "killfiles" gnus-directory)))
(setq gnus-message-archive-group "Sent")
(setq gnus-agent-directory (file-name-as-directory (expand-file-name "agent" gnus-directory)))
;; mark sent/Gcc mails as read
(setq gnus-gcc-mark-as-read t)
;; show more headers
(setq gnus-extra-headers '(To Cc Newsgroups))
;; not novice anymore, we think
(setq gnus-novice-user nil)
;; silent exit
(setq gnus-interactive-exit nil)
;; sort by date primarily
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date gnus-thread-sort-by-most-recent-number))
;; gnus-summary-line-format ":%U%R %B %s %-60=|%4L |%-20,20f |%&user-date; \n"
(setq gnus-summary-line-format "%U%R%z %&user-date; %I%(%[%4L: %-20,20n%]%) %S\n")
;; set date format
(setq gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M")))

;; often slow at rendering and when scrolling, especially when large
(setq gnus-inhibit-images t)

(setq gnus-icalendar-org-capture-file (expand-file-name "meetings.org" org-directory))
(setq gnus-icalendar-org-capture-headline '("Calendar"))
(gnus-icalendar-org-setup)

;; use same extra headers
(setq nnmail-extra-headers gnus-extra-headers)
(setq message-directory (file-name-as-directory (expand-file-name "Mail" gnus-directory)))
(setq nndraft-directory (file-name-as-directory (expand-file-name "drafts" message-directory)))
(setq nnfolder-directory (file-name-as-directory (expand-file-name "archive" message-directory)))
(setq nnfolder-active-file (expand-file-name "active" message-directory))
;; should be no more to comply with standards.
;; rfc5322#section-2.1.1
(setq message-fill-column 78)
(setq message-wash-forwarded-subjects t)
;; kill message after sending it
(setq message-kill-buffer-on-exit t)
;; dont read alias expansions from .mailrc
(setq message-mail-alias-type nil)
;; obsolete
(setq message-send-mail-function 'message-send-mail-with-sendmail)
;; warn if sending to an invalid email address
(setq message-setup-hook '(message-check-recipients))
;; determine the value of MFT headers. use built in gnus functions
(setq message-subscribed-address-functions '(gnus-find-subscribed-addresses))
;; extended citation line
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")
;; cursor position 'above' 'below' 'traditional' (inline)
;; newsgroups frown upon anything but inline
(setq message-cite-reply-position 'above)
;; dont autosave
(setq message-auto-save-directory nil)
;; according to docs, set to `t' should default to `mail-user-agent'
(setq message-mail-user-agent t)
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-queue-dir (expand-file-name "~/.mail/queue"))
;; debug sending email
(setq mm-default-directory (expand-file-name "~/dwl"))
(setq mm-tmp-directory (expand-file-name "~/tmp"))
;; verify known signed parts
(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)
;;; open a new window for composing email
(setq mml-secure-smime-sign-with-sender t)
(setq mml-secure-openpgp-sign-with-sender t)
;; investigate mbsync problem before updating indexes.
;; could be fatal error.
;;; hiding the message makes mu4e not display "Indexing ..." in
;;; minibuffer when mu gets updated as it can interrupt minibuffer
;;; usage.

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(setq sendmail-program (executable-find "msmtp"))
;; read who is sending the email

;; remove adding username --> msmtp takes care of this
(setq message-sendmail-f-is-evil t)
(setq smtpmail-stream-type 'starttls)
(setq smtpmail-smtp-service 587)

(defun my/gnus-mailto-compose-mail (mailto-url)
  "Parse MAILTO-URL and start composing mail."
  (require 'gnus)
  (unless gnus-active-hashtb (gnus))
  (if (and (stringp mailto-url)
           (string-match "\\`mailto:" mailto-url))
      (progn
        (require 'rfc6068)
        (require 'rfc2047)
        (require 'mailheader)

        (let ((hdr-alist (rfc6068-parse-mailto-url mailto-url))
              (body "")
              to subject
              ;; In addition to To, Subject and Body these headers are
              ;; allowed:
              (allowed-xtra-hdrs '(cc bcc in-reply-to)))
          (with-temp-buffer
            ;; Extract body if it's defined
            (when (assoc "Body" hdr-alist)
              (dolist (hdr hdr-alist)
                (when (equal "Body" (car hdr))
                  (insert (format "%s\n" (cdr hdr)))))
              (rfc2047-decode-region (point-min) (point-max))
              (setq body (buffer-substring-no-properties
                          (point-min) (point-max)))
              (erase-buffer))
            ;; Extract headers
            (dolist (hdr hdr-alist)
              (unless (equal "Body" (car hdr))
                (insert (format "%s: %s\n" (car hdr) (cdr hdr)))))
            (rfc2047-decode-region (point-min) (point-max))
            (goto-char (point-min))
            (setq hdr-alist (mail-header-extract-no-properties)))
          (setq to (or (cdr (assq 'to hdr-alist)) "")
                subject (or (cdr (assq 'subject hdr-alist)) "")
                hdr-alist
                (remove nil (mapcar
                             #'(lambda (item)
                                 (when (memq (car item) allowed-xtra-hdrs)
                                   (cons (capitalize (symbol-name (car item)))
                                         (cdr item))))
                             hdr-alist)))
          (gnus-setup-message 'message (message-mail to subject hdr-alist nil nil
                                                     (list (lambda (string)
                                                             (insert string))
                                                           body)))))
    (gnus-setup-message 'message (message-mail mailto-url))))



(setq image-use-external-converter t)



(defun mml-attach-file--go-to-eob (orig-fun &rest args)
  "Go to the end of buffer before attaching files."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (apply orig-fun args))))

;;; make mml write attachments to the end of the message
(advice-add 'mml-attach-file :around #'mml-attach-file--go-to-eob)


;; --- mu4e

(require 'mu4e nil t)
(require 'mu4e-icalendar nil t)
(require 'mu4e-contrib nil t)
;;; it seems mu4e moved obsolete variables to this file. use it to
;;; check their new names
(require 'mu4e-obsolete nil t)

(require 'mu4e-config nil t)

(defvar mu4e-major-version 0 "mu4e major version number.")
(defvar mu4e-minor-version 0 "mu4e minor version number.")

(when (boundp 'mu4e-mu-version)
  (let* ((ver-list (version-to-list mu4e-mu-version))
         (major (nth 0 ver-list))
         (minor (nth 1 ver-list)))
    (setq mu4e-major-version major)
    (setq mu4e-minor-version minor)))

(if (and (> mu4e-major-version 0) (> mu4e-minor-version 11))
    (setq mu4e-compose-switch t)
  (setq mu4e-compose-in-new-frame t))

(setq mu4e-hide-index-messages t)
(setq mu4e-index-update-status t)
(setq mu4e-index-update-error-warning t)
(setq mu4e-index-update-error-continue nil)
(setq mu4e-attachment-dir (format "%s/dwl" (getenv "HOME")))
;; should be better interop with mbsync
(setq mu4e-change-filenames-when-moving t)
;; mbsync can make changes that escape a 'lazy' check
(setq mu4e-index-lazy-check t)
(if (and (> mu4e-major-version 0) (< mu4e-minor-version 12))
    (setq mu4e-compose-dont-reply-to-self t))

;; more pleasant reading flow view
(setq mu4e-compose-format-flowed nil)
(setq mu4e-confirm-quit nil)
(setq mu4e-completing-read-function 'completing-read)
;; mu4e-index-update-in-background nil
(setq mu4e-icalendar-diary-file (format "%s/mu4e-diary" (getenv "HOME")))
(setq mu4e-icalendar-trash-after-reply t)
(setq mail-user-agent 'mu4e-user-agent)
;;; mu4e-update-index should happen after command is finished
(setq mu4e-get-mail-command "mu4e-get-mail-command.sh")


(setq mu4e-sent-folder "/local/Sent")
(setq mu4e-trash-folder "/local/Trash")
(setq mu4e-refile-folder "/local/Archive")
(setq mu4e-drafts-folder "/local/Drafts")
;; if not `pick-first' mu4e will ask for context on first startup
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'ask-if-none)
(setq mu4e-user-agent-string nil)

(setq mu4e-headers-date-format "%d/%m/%Y %H:%M")
(setq mu4e-compose-signature-auto-include nil)
(setq mu4e-headers-fields '((:human-date . 18)
			                (:flags      . 6)
			                (:maildir    . 16)
			                (:from-or-to . 26)
			                (:subject    . 90)))
(setq mu4e-search-results-limit 2000)
;;; set mu4e as default mail reader
(setq read-mail-command 'mu4e)

(setq bbdb-mail-user-agent 'mu4e-user-agent)


(setq mu4e-bookmarks '((:name "Unread messages (not trash/spam)"
                              :query "flag:unread AND NOT flag:trashed AND NOT maildir:/Junk/ AND NOT maildir:/Trash/ AND NOT maildir:/Spam/"
                              :key ?u)
                       (:name "Flagged"
                              :query "flag:flagged"
                              :key ?f)
                       (:name "Inbox"
                              :query "maildir:/Inbox/ AND maildir:/INBOX/"
                              :key ?i)
                       (:name "Today"
                              :query "date:today..now AND NOT maildir:/Junk/ AND NOT maildir:/Trash/ AND NOT maildir:/Spam/"
                              :key ?t)))

(defun my/mu4e-quit()
  (interactive)
  (if (and (derived-mode-p 'mu4e-main-mode))
      (progn
        (mu4e-quit)
        (delete-frame (selected-frame)))
    (mu4e-quit)))

(defun my/mu4e-update-index()
  (interactive)
  (message "Updating mu4e index ...")
  (mu4e-update-index-nonlazy)
  (revert-buffer nil t t)
  (message "mu4e index updated"))


(defun my/message-compose-hook ()
  (let ((msg mu4e-compose-parent-message))
    (when msg
      (let ((msg-list-id (mu4e-message-field msg :list)) ; probably always string
            (msg-list-post (mu4e-message-field msg :list-post)) ; sometimes (cons(cons))
            (msg-to (mu4e-message-field msg :to)))
        (when (or msg-list-id msg-list-post)
          ;; reply inline when responding to lists
          (setq-local message-cite-reply-position 'traditional))
        (when (or (and msg-to (or (seq-find (lambda (elt) (string-match-p "\.*openbsd.org" (car (cdr elt)))) msg-to)
                                  (seq-find (lambda (elt) (string-match-p "\.*opensmtpd.org" (car (cdr elt)))) msg-to)))
                  (and msg-list-id (string-match-p "\.*gnu.org" msg-list-id))
                  (and msg-list-post (seq-find (lambda (elt) (string-match-p "\.*gnu.org" (car (cdr elt)))) msg-list-post)))
          (setq-local message-fill-column 72)
          (setq-local fill-column 72))))))

(org-save-all-org-buffers)

(when (require 'mu4e nil 'noerror)
  (load-file (expand-file-name "init-mu4e.el" user-emacs-directory))
  (require 'init-mu4e)

  (define-key mu4e-headers-mode-map (kbd "M") #'mu4e-headers-mark-all)
  (define-key mu4e-headers-mode-map (kbd "N") #'mu4e-headers-mark-all-unread-read)
  (define-key mu4e-compose-mode-map (kbd "M-q") #'fill-paragraph)
  (define-key mu4e-main-mode-map (kbd "q") #'my/mu4e-quit)
  (define-key mu4e-main-mode-map (kbd "g") #'my/mu4e-update-index)
  (add-hook 'mu4e-view-mode-hook #'epa-mail-decrypt)
  ;; (add-hook 'mu4e-context-changed-hook (lambda ()
  ;;                                        (when (derived-mode-p 'mu4e-main-mode)
  ;;                                          (revert-buffer))))

  (defun my/mu4e-browse-url-mail (url)
    (if (mu4e-running-p)
        (browse-url-mail url)
      (user-error "mu4e not running. cannot compose mailto")))


  (defun my/mu4e-update-index-if-running ()
    (interactive)
    (if (mu4e-running-p)
        (mu4e-update-index-nonlazy)))



  (defun my/switch-to-or-open-mu4e ()
    (interactive)
    (let (mu4e-main-buffer (get-buffer "*mu4e-main*"))
      (if mu4e-main-buffer
          (switch-to-buffer mu4e-main-buffer)
        (mu4e))))

  ;; (require 'mu4e-draft)


  (add-hook 'mu4e-compose-mode-hook #'my/message-compose-hook)

  ;; enable bbdb in needed packages
  (bbdb-initialize 'message 'mu4e))



(require 'notmuch)

(defvar notmuch-cache-dir (expand-file-name "~/.cache/notmuch") "Custom default directory to store Notmuch realated files.")

(setq notmuch-show-logo nil)
(setq notmuch-show-all-tags-list t)
(setq notmuch-search-oldest-first nil)
(setq notmuch-mua-cite-function #'message-cite-original-without-signature)
(setq notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                               (:name "unread" :query "tag:unread and not tag:list" :key "u")
                               (:name "unread-list" :query "tag:unread and tag:list" :key "l")
                               (:name "flagged" :query "tag:flagged" :key "f")
                               (:name "sent" :query "tag:sent" :key "t")
                               (:name "drafts" :query "tag:draft" :key "d")
                               (:name "junk" :query "tag:junk" :key "j")
                               (:name "all mail" :query "*" :key "a")))
;; Cache addresses for completion:
(setq notmuch-address-save-filename (concat notmuch-cache-dir "/addresses"))
(setq notmuch-archive-tags '("-inbox" "-unread" "+archive"))
;; using notmuch insert does not seem to mark sent mail as read.
;; maybe it is a bug. by setting this nil notmuch unfortunately
;; also do not index the recently sent mail
(setq notmuch-maildir-use-notmuch-insert nil)
(setq notmuch-unthreaded-show-out nil)

(make-directory notmuch-cache-dir t)




;; --- git
;; basic tools for handling git files and git repos
(require 'magit)
(require 'magit-extras)

(setq magit-blame-echo-style 'margin)
;; don't require save buffers before visiting git repo
(setq magit-save-repository-buffers nil)

(global-set-key (kbd "C-x g") 'magit-status)


;;; using `magit-bury-buffer-function' makes magit bury all magit
;;; buffers.
;; (defun my/magit-kill-buffers ()
;;   "Restore window configuration and kill all Magit buffers."
;;   (interactive)
;;   (let ((buffers (magit-mode-get-buffers)))
;;     (magit-restore-window-configuration)
;;     (mapc #'kill-buffer buffers)))

;; (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'my/magit-kill-buffers)

;; restore previous window setup when burying
(setq magit-bury-buffer-function #'magit-restore-window-configuration)

(add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m) t)


(require 'git-modes)
(require 'gitignore-templates)

(defvar my:ticket-prefix "osm-")

(defvar my:git-commit-prefix "")

(defun set-osm-ticket()
  "Asks for user input to populate git commit prefix.
Ticket IDs should be separated with whitespaces."
  (interactive)
  (let* ((somevar (read-from-minibuffer "input: "))
         (spl (split-string somevar)))
    (if spl
        (setq my:git-commit-prefix (format "%s%s" my:ticket-prefix (string-join spl (format "/%s" my:ticket-prefix))))
      (setq my:git-commit-prefix ""))))

(defun append-git-commit-prefix()
  "Append defined prefix into buffer."
  (if (> (length my:git-commit-prefix ) 3)
      (insert (format "%s: " my:git-commit-prefix))))


;; add prefix to commit message if exists
(add-hook 'git-commit-setup-hook #'append-git-commit-prefix)


(add-to-list 'auto-mode-alist '("\\.gitconfig.*\\'" . gitconfig-mode))



(require 'expand-region)

(global-set-key (kbd "C-;") 'er/contract-region)
(global-set-key (kbd "C-'") 'er/expand-region)

;; --- prog mode
;; general config for programming modes

(require 'editorconfig)

(setq editorconfig-mode-lighter " ec")

;; --- yank
(require 'yank-indent nil t)

;; only enable on select modes instead of `prog-mode-hook'.
;; some modes might implement this feature already
(when (fboundp 'yank-indent-mode)
  (add-hook 'c-mode-hook #'yank-indent-mode)
  (add-hook 'c++-mode-hook #'yank-indent-mode))


;; color delimiters differently
(require 'rainbow-delimiters)
(require 'highlight-escape-sequences)
(require 'yasnippet)
(require 'consult-yasnippet)

(diminish 'yas-minor-mode)

;;; as per doc, needed if global-mode is not used
(yas-reload-all)

;; indent with whitespace by default to make files appear the same
;; across editors
(setq-default indent-tabs-mode nil
              ;; set tab width
              tab-width 4)

(require 'elec-pair)

(setq electric-pair-inhibit-predicate 'electric-pair-default-inhibit)
(setq electric-pair-preserve-balance t)
(setq electric-pair-skip-self 'electric-pair-default-skip-self)
(setq electric-pair-skip-whitespace nil)
(setq electric-quote-context-sensitive t)
(setq electric-quote-paragraph t)
(setq electric-quote-string nil)
(setq electric-quote-replace-double t)

(add-to-list 'electric-pair-pairs '(?\[ . ?\]))

(electric-quote-mode -1)


;; add eletric pair only on needed modes. elisp modes have other
;; should not be intruding any modes.
;;

;; (add-hook 'prog-mode-hook #'electric-pair-mode)
;; (add-hook 'prog-mode-hook #'electric-indent-local-mode)

;; show matching parenthesis
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'turn-on-hes-mode)
(add-hook 'prog-mode-hook #'yas-minor-mode)



;; --- eglot

(require 'eglot)

;; only trigger send-changes on save for now. otherwise make seems
;; to check whenever changes are made despite this should have been
;; disabled using `flymake-no-changes-timeout'
;; (setq eglot-send-changes-idle-time nil)

;;; reconnect if lsp server shuts down
;; (setq eglot-autoreconnect nil)
;;; shutdown lsp servers when not needed
(setq eglot-autoshutdown t)
(setq eglot-server-programs (delq (assoc '(python-mode python-ts-mode) eglot-server-programs) eglot-server-programs))
(setq eglot-server-programs (delq (assoc '(c-mode c++-mode) eglot-server-programs) eglot-server-programs))

(add-to-list 'eglot-server-programs (list '(f90-mode) "fortls" "--notify_init" "--nthreads=4"))
(add-to-list 'eglot-server-programs (list '(toml-ts-mode) "taplo-full" "lsp" "stdio" "--colors" "never"))
(add-to-list 'eglot-server-programs (list '(nxml-mode) "lemminx"))
(add-to-list 'eglot-server-programs (list '(markdown-mode) "marksman"))

;; (add-to-list 'eglot-server-programs (list '(c-mode c++-mode) "clangd" "--limit-results=900" "--completion-style=detailed" (format "-j=%d" (num-processors)) "--header-insertion=never" "--header-insertion-decorators=false" "--pch-storage=memory"))
(add-to-list 'eglot-server-programs
             `((python-mode python-ts-mode) . ,(eglot-alternatives
                                                '(("jedi-language-server" :initializationOptions (:diagnostics (:enable nil)
                                                                                                               ;; disable snippets to avoid TAP complete/expand almost every thing into a function call with arguments
                                                                                                               :completion (:disableSnippets t)
                                                                                                               :jediSettings (:debug nil)))
                                                  ("pylsp")))))


(defun my/restart-eglot-server ()
  (interactive)
  (eglot-shutdown (eglot-current-server))
  (eglot-ensure))



;;; --- emacs lisp
(require 'etags)
(require 'paredit)
(require 'flymake-elisp-config)

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(defun sanityinc/load-this-file ()
  "Load the current file or buffer.
The current directory is temporarily added to `load-path'.  When
there is no current file, eval the current buffer."
  (interactive)
  (let ((load-path (cons default-directory load-path))
        (file (buffer-file-name)))
    (if file
        (progn
          (save-some-buffers nil (apply-partially 'derived-mode-p 'emacs-lisp-mode))
          (load-file (buffer-file-name))
          (message "Loaded %s" file))
      (eval-buffer)
      (message "Evaluated %s" (current-buffer)))))

(defun sanityinc/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))

(define-key paredit-mode-map (kbd "M-?") #'xref-find-references)

(require 'eros)


(defun init-elisp-mode()
  ;; (face-remap-add-relative 'default :height 0.8)
  (setq mode-name "ELisp")

  ;; add sections. `imenu-generic-expression' is buffer local.
  ;; FIXME: create proper category instead of multiple "Sections" -prefixed entries.
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;? ---?\\(.+\\)" 1))
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  ;; use Makefile to generate TAGS file from Emacs Lisp from both
  ;; system installed lisp and user packages
  (setq tags-table-list '("~/.emacs.d/"))
  (let* ((filename (buffer-file-name))
         ;; quick way to determine whether file resides in an .emacs.d directory
         (emacs-config (and filename (locate-dominating-file (file-name-directory filename) "init.el")))
         (dom-etags-file (and filename (locate-dominating-file (file-name-directory filename) "TAGS"))))
    (cond (emacs-config
           (flymake-elisp-config-as-config (current-buffer)))
          (t
           (flymake-elisp-config-as-default)))
    (when dom-etags-file
      (add-to-list 'tags-table-list dom-etags-file))))

;; flymake is not populated with all relevant paths during startup
;; so we set it manually
;; (setq elisp-flymake-byte-compile-load-path load-path)

(add-hook 'emacs-lisp-mode-hook #'init-elisp-mode)
;; (add-hook 'emacs-lisp-mode-hook #'flymake-mode)
                                        ;
;; nice to have evaluated result next to cursor on eval-*
(add-hook 'emacs-lisp-mode-hook #'eros-mode)

;; if browsing elisp code bundles with emacs we are usually not
;; interested in edit it
(add-hook 'emacs-lisp-mode-hook #'sanityinc/maybe-set-bundled-elisp-readonly)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(global-set-key [remap eval-expression] 'pp-eval-expression)
(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'sanityinc/load-this-file)

(require 'ielm)

(defun init-ielm-mode-hook ()
  (turn-on-eldoc-mode)
  (and (featurep 'paredit) (enable-paredit-mode)))

(add-hook 'ielm-mode-hook #'init-ielm-mode-hook)
(add-hook 'ielm-mode-hook #'corfu-mode)


;; --- shell-script
(require 'sh-script)

;; void linux package templates are shell scripts according to manual
(add-to-list 'auto-mode-alist '("/template\\'" . shell-script-mode))
(add-hook 'sh-mode-hook #'electric-pair-local-mode)

;; --- common lisp
(require 'slime)

(setq inferior-lisp-program "sbcl")

(slime-setup '(slime-repl slime-fuzzy))

(add-to-list 'slime-lisp-implementations
             '(sbcl ("sbcl") :coding-system utf-8-unix))



;; --- clojure
(require 'cider)
(require 'clj-refactor)
(require 'flymake-kondor)

(setq cider-repl-display-help-banner nil)

;; NOTICE: cider does not seem to enable by default as stated in docs
(add-hook 'clojure-mode-hook #'cider-mode)
(add-hook 'clojure-mode-hook #'flymake-kondor-setup)
(add-hook 'clojure-mode-hook #'flymake-mode)
;; as we often deal with java in clojure this should be helpful
(add-hook 'cider-repl-mode-hook #'subword-mode)
;; highlight parentheses
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)




(require 'gdb-mi)

(setq gdb-many-windows t)
(setq gdb-debuginfod-enable-setting nil)


;; useful to define small reformatter tools when code editing
(require 'reformatter)



;; --- c/cpp modes

(require 'cc-mode)
(require 'cc-vars)
(require 'c-ts-mode)
(require 'flymake-cppcheck nil t)
(require 'openbsd-knf-style)
(require 'apache-c-style)
(require 'fluent-bit-c-style)
(require 'gnu-indent)
(require 'citre)
(require 'clang-format)

;; (defconst c-ts-mode--c-or-c++-regexp
;;   (eval-when-compile
;;     (let ((id "[a-zA-Z_][a-zA-Z0-9_]*") (ws "[ \t]+") (ws-maybe "[ \t]*")
;;           (headers '("string" "chrono" "functional" "string_view" "iostream" "map" "unordered_map"
;;                      "set" "unordered_set" "vector" "tuple")))
;;       (concat "^" ws-maybe "\\(?:"
;;               "using"     ws "\\(?:namespace" ws
;;               "\\|" id "::"
;;               "\\|" id ws-maybe "=\\)"
;;               "\\|" "\\(?:inline" ws "\\)?namespace"
;;               "\\(:?" ws "\\(?:" id "::\\)*" id "\\)?" ws-maybe "{"
;;               "\\|" "class"     ws id
;;               "\\(?:" ws "final" "\\)?" ws-maybe "[:{;\n]"
;;               "\\|" "struct"     ws id "\\(?:" ws "final" ws-maybe "[:{\n]"
;;               "\\|" ws-maybe ":\\)"
;;               "\\|" "template"  ws-maybe "<.*?>"
;;               "\\|" "#include"  ws-maybe "<" (regexp-opt headers) ">"
;;               "\\)"))))

;;; -nut = no tabs
;;; -sc = add * left side of C comment start
;;; -sob = delete excess blank lines
(setq gnu-indent-options '("-kr" "-nut" "-sc" "-sob" "-psl"))

;; (setq-default c-ts-mode-indent-style 'k&r)
;; defaults to '2'
(setq-default c-ts-mode-indent-offset 4)


(require 'cl-lib)

;; (defun init-c-ts-common-mode ()
;;   (setq-local eglot-stay-out-of '(flymake))
;;   (setq-local flymake-diagnostic-functions nil)
;;   (setq-local compile-command "make -C build")
;;   (let* ((filename (buffer-file-name))
;;          (dom-tags-file (and filename (locate-dominating-file (file-name-directory filename) "TAGS")))
;;          (dom-comp-cmd-file (and filename (locate-dominating-file filename "compile_commands.json")))
;;          (dom-kconfig-file (and filename (or (locate-dominating-file filename "Kbuild")
;; 		                                     (locate-dominating-file filename "Kconfig")
;; 		                                     (save-excursion (goto-char 0)
;; 				                                             (search-forward-regexp "^#include <linux/\\(module\\|kernel\\)\\.h>$" nil t))))))
;;     (when (and dom-comp-cmd-file (not dom-kconfig-file))
;;       (eglot-ensure)
;;       (if (string-match-p "fluent-bit" (file-name-directory (buffer-file-name)))
;;           ;; likely in a fluent-bit project. use gnu indent for formatting instead
;;           (progn
;;             (setq-local gnu-indent-options fluent-bit-c-gnu-indent-options)
;;             (define-key c-ts-base-mode-map my/format-kbd #'gnu-indent-buffer))
;;         (define-key c-ts-base-mode-map my/format-kbd #'eglot-format))

;;       (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend -100 t))
;;     ;; manually add eglot to flymake to have it not replace
;;     ;; existing functions.
;;     (if dom-tags-file
;;         (add-to-list 'tags-table-list dom-tags-file))))


(setq citre-tags-completion-case-sensitive nil)
(setq citre-global-completion-case-sensitive nil)

;;; NOTE: protobuf-mode inherits from cc-mode
(defun init-c-common-mode ()
  ;; (face-remap-add-relative 'default :height 0.8)
  ;; when used in c-mode emacs sometimes complains with existing
  ;; diagnostic functions state not found within
  ;; `flymake--state'. `eglot-ensure' seems to trigger this hence why
  ;; disable its meddling with flymake.
  (setq-local eglot-stay-out-of '(flymake))
  (setq-local flymake-diagnostic-functions nil)

  ;; might as well default compile command to make
  (setq-local compile-command "make -C build")

  ;; macros that should or must not end with semicolon and therefore
  ;; causes indentation mistakes
  (add-to-list 'c-macro-names-with-semicolon "TAU_MAIN")

  ;; make clang-format the default formatter
  (local-set-key my/format-kbd #'clang-format-buffer)

  (let* ((filename (buffer-file-name))
         (fluent-bit-source (and filename (string-match-p "fluent-bit" (file-name-directory (buffer-file-name)))))
         (dom-etags-file (and filename (locate-dominating-file (file-name-directory filename) "TAGS")))
         (dom-ctags-file (and filename (locate-dominating-file (file-name-directory filename) "tags")))
         (dom-comp-cmd-file (and filename (locate-dominating-file filename "compile_commands.json")))
         (dom-kconfig-file (and filename (or (locate-dominating-file filename "Kbuild")
		                                     (locate-dominating-file filename "Kconfig")
		                                     (save-excursion (goto-char 0)
				                                             (search-forward-regexp "^#include <linux/\\(module\\|kernel\\)\\.h>$" nil t))))))

    ;; set style and indentation based on conditionals.
    (cond (dom-kconfig-file
           ;; likely in a linux kernel source tree
           (c-set-style "linux")
           (setq indent-tabs-mode t)
           (setq c-basic-offset 8)
           (setq tab-width 8))
          (fluent-bit-source
           ;; likely in a fluent-bit project.
           (c-set-style "fluent-bit-c")
           (setq-local tab-width 4)
           (setq c-basic-offset 4)
           (setq-local gnu-indent-options fluent-bit-c-gnu-indent-options)
           ;; use gnu indent for formatting.
           (local-set-key my/format-kbd #'gnu-indent-buffer))
          (t
           (if (eq major-mode 'c-mode)
               (c-set-style "k&r"))
           (if (eq major-mode 'c++-mode)
               (c-set-style "stroustrup"))
           (setq-local indent-tabs-mode nil)
           (setq-local tab-width 4)
           ;; k&r defaults to 5 but gnu-indent '-kr' defaults to 4 :-|
           (setq c-basic-offset 4)))

    ;; only enable either eglot or tags based backends based on conditionals.
    ;; only one should be needed at a time.
    (cond (dom-comp-cmd-file
           (eglot-ensure)
           (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend -100 t)
           (when (not fluent-bit-source)
               ;; use eglot backed formatter when eglot is used,
               ;; except fluent-bit source code which is formatted
               ;; with gnu-indent
             (local-set-key my/format-kbd #'eglot-format))
           (flymake-mode t))
          (dom-ctags-file
           (citre-mode t)
           (flymake-cppcheck-setup)
           (flymake-mode t)
           (local-set-key (kbd "M-,") #'citre-jump-back)
           (local-set-key (kbd "M-.") #'citre-jump)
           (local-set-key (kbd "M-?") #'citre-jump-to-reference))
          (dom-etags-file
           (add-to-list 'tags-table-list dom-etags-file)))))

(defun uncrustify-buffer ()
  "Run caddy formatting on the current region or buffer."
  (interactive)

  (let* ((current-line (line-number-at-pos))
         (start (point-min))
         (dom-cfg (locate-dominating-file (buffer-file-name (current-buffer)) ".uncrustify.cfg"))
         (uncrustify-cfg (if dom-cfg dom-cfg "~/.uncrustify.cfg"))
         (uncrustify-exec (executable-find "uncrustify")))
    (unless uncrustify-exec
      (error "could not find uncrustify in PATH"))
    (unless uncrustify-cfg
      (error "could not find dominating cfg file"))
    (unless (not (file-exists-p uncrustify-cfg))
      (error (format "could not uncrustify config %s does not exist" uncrustify-cfg)))

    (shell-command-on-region start (point-max) (format "%s -q -c %s -l C" uncrustify-exec uncrustify-cfg) nil t nil t)
    (goto-char start)
    (forward-line (1- current-line))))


(defun my/indent-whole-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun my/indent-style()
  "Override the built-in BSD indentation style with some additional rules"
  `(;; Here are your custom rules
    ((node-is ")") parent-bol 0)
    ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
    ((parent-is "argument_list") prev-sibling 0)
    ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
    ((parent-is "parameter_list") prev-sibling 0)

    ;; Append here the indent style you want as base
    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))

(setq-default c-ts-mode-indent-style #'my/indent-style)

(define-key c-mode-map my/compile-kbd #'recompile)
(define-key c-mode-map my/comment-kbd #'comment-dwim)
(define-key c-ts-mode-map my/compile-kbd #'recompile)
(define-key c-ts-mode-map my/comment-kbd #'comment-dwim)


(define-key c++-mode-map my/comment-kbd #'comment-dwim)
(define-key c++-mode-map my/compile-kbd #'recompile)
(define-key c++-mode-map my/format-kbd #'eglot-format)
(define-key c++-ts-mode-map my/format-kbd #'eglot-format)



(add-hook 'c-mode-hook #'init-c-common-mode)
(add-hook 'c-mode-hook #'electric-indent-local-mode)
(add-hook 'c-mode-hook #'electric-pair-local-mode)
;; (add-hook 'c-mode-hook #'flymake-mode)
;; (add-hook 'c-mode-hook #'breadcrumb-local-mode)
;; (add-hook 'c-ts-mode-hook #'init-c-ts-common-mode)
;; (add-hook 'c-ts-mode-hook #'flymake-mode)
(add-hook 'c++-mode-hook #'init-c-common-mode)
(add-hook 'c++-mode-hook #'electric-indent-local-mode)
(add-hook 'c++-mode-hook #'electric-pair-local-mode)
;; (add-hook 'c++-mode-hook #'flymake-mode)
;; (add-hook 'c++-mode-hook #'breadcrumb-local-mode)
;; (add-hook 'c++-ts-mode-hook #'init-c-ts-common-mode)
;; (add-hook 'c++-ts-mode-hook #'flymake-mode)

(define-key c-mode-map my/compile-kbd #'recompile)
(define-key c++-ts-mode-map my/compile-kbd #'recompile)

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c-or-c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.\\(CC?\\|HH?\\)\\'" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.\\(cc\\|hh\\)\\'" . c++-mode))

;; disable -ts-modes for now. too many issue on different OS platforms

;;; there are multiple associations for `c-ts-mode'
(while-let ((mode-assoc (rassoc 'c-ts-mode auto-mode-alist)))
  (if mode-assoc
      (setf (cdr mode-assoc) 'c-mode)))

(while-let ((mode-assoc (rassoc 'c++-ts-mode auto-mode-alist)))
  (if mode-assoc
      (setf (cdr mode-assoc) 'c++-mode)))

(while-let ((mode-assoc (rassoc 'c-or-c++-ts-mode auto-mode-alist)))
  (if mode-assoc
      (setf (cdr mode-assoc) 'c-or-c++-mode)))


;;; protobuf inherits from cc-mode
(require 'protobuf-mode)


;; --- rust
(require 'rust-mode)
(require 'cargo-mode)

(defun init-rust-mode ()
  ;; use white space according to rust standards
  (setq-local indent-tabs-mode nil)
  ;; lets format by default on save
  (setq rust-format-on-save t))

(add-hook 'rust-mode-hook #'init-rust-mode)
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'rust-mode-hook #'electric-pair-local-mode)

;; --- golang
(require 'go-mode)
(require 'go-dlv)

(require 'go-ts-mode)

(reformatter-define go-format
  :group 'go-ts-mode
  :program "ts"
  :args '("/dev/stdin"))
(add-hook 'go-ts-mode-hook #'eglot-ensure)
;; (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))


(add-hook 'go-mode-hook #'eglot-ensure)


;; --- shell script

(require 'shfmt)
(require 'sh-script)

;; use whitespace, not tabs
;; eglot + bash-language-server doesn't seem to support format buffer
(setq shfmt-arguments (list "-i" (format "%d" tab-width)))

(define-key sh-mode-map my/format-kbd #'shfmt-buffer)
(add-hook 'sh-mode-hook #'flymake-mode)


;; --- lua mode
(require 'lua-mode)

;; add luarock files as well
(add-to-list 'auto-mode-alist '("\\.rockspec" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.busted" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.slua" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;; latest lua-mode from immerrr/lua-mode supports flymake
(add-hook 'lua-mode-hook #'flymake-mode)

;; --- python mode
(require 'python)
(require 'pip-requirements)
(require 'pyvenv)
(require 'pyvenv-auto)
(require 'flymake-ruff)
(require 'python-black)
(require 'numpydoc)
(require 'importmagic)

(reformatter-define ruff-format
  :program "ruff"
  :args (list "format" "--stdin-filename" (or (buffer-file-name) input-file))
  :group 'ruff-format)

(defun init-python-mode()
  (setq-local eglot-stay-out-of '(flymake))
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend -100 t)
  (eglot-ensure))

(setq python-indent-guess-indent-offset nil)
(setq numpydoc-insertion-style 'prompt)
(setq numpydoc-template-long "")
(setq numpydoc-template-short "")
(setq numpydoc-template-arg-desc "")
(setq numpydoc-template-type-desc "")
(setq numpydoc-insert-examples-block nil)
(setq numpydoc-insert-return-without-typehint t)

;; add init before lsp to disable some lsp features
(add-hook 'python-mode-hook #'pyvenv-auto-run)
(add-hook 'python-mode-hook #'flymake-ruff-load)
(add-hook 'python-mode-hook #'subword-mode) ; become snake-case aware
(add-hook 'python-mode-hook #'init-python-mode)
(add-hook 'python-mode-hook #'flymake-mode)
(add-hook 'python-mode-hook #'electric-pair-local-mode)
(add-hook 'python-mode-hook #'electric-indent-local-mode)


(define-key python-mode-map (kbd "C-c C-n") #'numpydoc-generate)
(define-key python-mode-map (kbd "C-c b") #'python-shell-send-buffer)
(define-key python-mode-map my/format-kbd #'ruff-format-buffer)


(add-hook 'python-ts-mode-hook #'pyvenv-auto-run)
(add-hook 'python-ts-mode-hook #'flymake-ruff-load)
(add-hook 'python-ts-mode-hook #'init-python-mode)
(add-hook 'python-ts-mode-hook #'flymake-mode)
(add-hook 'python-ts-mode-hook #'electric-pair-local-mode)
(add-hook 'python-ts-mode-hook #'electric-indent-local-mode)

(define-key python-ts-mode-map (kbd "C-c C-n") #'numpydoc-generate)
(define-key python-ts-mode-map (kbd "C-c b") 'python-shell-send-buffer)
(define-key python-ts-mode-map my/format-kbd #'ruff-format-buffer)

(add-to-list 'auto-mode-alist '("\\.py[iw]?\\'" . python-ts-mode))


(setq-default eglot-workspace-configuration
              '((:pylsp . (:plugins (:flake8 (:enabled nil)
                                             :rope_autoimport (:enabled nil)
                                             :isort (:enabled t)
                                             :pycodestyle (:enabled t)
                                             :pylint (:enabled nil)
                                             :jedi_completion (:fuzzy t))))
                (:haskell . (:plugin (:stan (:globalOn nil))))
                (:gopls
                 (staticcheck . t)
                 (matcher . "CaseSensitive"))))




(require 'rst)

;;; rst files often needs rst2html.py to compile
(add-hook 'rst-mode-hook #'pyvenv-mode)
(add-hook 'rst-mode-hook #'pyvenv-auto-run)


;; --- json mode
(require 'json-ts-mode)

;; --- json mode
(require 'flymake-jsonlint nil t)
(require 'jq-mode)

(defun init-json-mode()
  (setq tab-width 2))

(reformatter-define python-json-tool-format
  :program "python"
  :args (if indent-tabs-mode (list "-mjson.tool" "--tab")
		  (list "-mjson.tool" "--indent" (format "%d" tab-width)))
  :group 'python-json-tool-format)


(reformatter-define python-json-tool-compact
  :program "python"
  :args (list "-mjson.tool" "--compact")
  :group 'python-json-tool-compact)


(defun compact-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool --compact"  (current-buffer) nil t)))


(add-to-list 'auto-mode-alist '("\\.json" . json-ts-mode))
(add-to-list 'auto-mode-alist '(".*/waybar/config.*" . json-ts-mode))

(define-key json-ts-mode-map my/format-kbd #'python-json-tool-format-buffer)
(define-key json-ts-mode-map (kbd "C-c C-j") #'jq-interactively)

(add-hook 'json-ts-mode-hook #'init-json-mode)
(if (fboundp 'flymake-jsonlint-setup)
    (add-hook 'json-ts-mode-hook #'flymake-jsonlint-setup))
(add-hook 'json-ts-mode-hook #'flymake-mode)
(add-hook 'json-ts-mode-hook #'electric-indent-local-mode)
(add-hook 'json-ts-mode-hook #'electric-pair-local-mode)


;;; --- yaml
;; yaml-mode does some commands better than yaml-ts-mode, line newline-indent
(require 'yaml-mode)
;;; 1.0.3 failed compilation with: yaml-imenu.el: Error: Wrong type argument proper-list-p
;; (require 'yaml-imenu)
;; used for yaml mainly
(require 'highlight-indentation)

(require 'flymake-yamllint nil t)

(defun init-yaml-mode()
  ;; tabs are outlawed
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local highlight-indentation-offset 2))

(reformatter-define yamlfmt
  :program "yamlfmt"
  :args (list "-in")
  :group 'yamlfmt)



(add-hook 'yaml-mode-hook #'init-yaml-mode)

(if (fboundp 'flymake-yamllint-setup)
    (add-hook 'yaml-mode-hook #'flymake-yamllint-setup))

(add-hook 'yaml-mode-hook #'flymake-mode)
(add-hook 'yaml-mode-hook #'highlight-indentation-mode)

(define-key yaml-mode-map my/format-kbd #'yamlfmt-buffer)

(add-to-list 'auto-mode-alist '("\\.yamllint" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.clang-format" . yaml-mode))



;;; --- kconfig mode
(require 'kconfig-mode)

(add-to-list 'auto-mode-alist '("\\.*ptxconfig.*\\'" . kconfig-mode))
(add-to-list 'auto-mode-alist '("\\.*platformconfig\\'" . kconfig-mode))


;; --- xml
(require 'nxml-mode)
(require 'xml-format)
;; convenience. rename start/end tags when altering
(require 'auto-rename-tag)

(require 'flymake-xmllint nil t)


(add-hook 'nxml-mode-hook #'hs-minor-mode)
(if (fboundp 'flymake-xmllint-setup)
    (add-hook 'nxml-mode-hook #'flymake-xmllint-setup))
(add-hook 'nxml-mode-hook #'flymake-mode)


(add-to-list 'hs-special-modes-alist (list 'nxml-mode
                                           "<!--\\|<[^/>]*[^/]>"
                                           "-->\\|</[^/>]*[^/]>"
                                           "<!--"
                                           'nxml-forward-element
                                           nil))

(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

;; to remove the "Valid/Invalid" from nxml mode line. Replaced by
;; flymake.
(setq rng-nxml-auto-validate-flag nil)

(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(setq nxml-slash-auto-complete-flag t)

(defun nxml-pretty-format ()
  "."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region 0 (count-lines (point-min) (point-max))))
  )

(defun tidy-buffer-xml (beg end)
  "Run \"tidy -xml\" on the region from BEG to END, or whole buffer."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  (shell-command-on-region beg end "tidy -xml -q -i" (current-buffer) t "*tidy-errors*" t))


(define-key nxml-mode-map my/format-kbd #'xml-format-buffer)
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.scxml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sch\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rng\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.svg\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rss\\'" . nxml-mode))

(add-to-list 'flyspell-prog-text-faces 'nxml-text-face)




;; --- css-mode
(require 'css-mode)

(setq css-indent-offset 2)

(add-hook 'css-mode-hook #'rainbow-mode)



;; --- web-mode
;; (require 'web-mode)
;; nice to have
(require 'rainbow-mode)

;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))


;; (setq web-mode-markup-indent-offset 2)
;; (setq web-mode-css-indent-offset 2)
;; (setq web-mode-code-indent-offset 4)
;; (setq web-mode-enable-auto-pairing t)
;; (setq web-mode-enable-auto-closing t)
;; (setq web-mode-enable-auto-opening t)
;; (setq web-mode-enable-auto-indentation t)
;; (setq web-mode-enable-comment-interpolation t)
;; (setq web-mode-enable-heredoc-fontification t)
;; ;; WARNING: these two highlights seems to mess up when in tty. nil if needed.
;; (setq web-mode-enable-current-element-highlight t)
;; (setq web-mode-enable-current-column-highlight t)
;; (setq web-mode-engines-alist '(("php"    . "\\.phtml\\'")
;;                                ("blade"  . "\\.blade\\.")
;;                                ("django"  . "\\.jinja\\'")))


;; (defun init-web-mode-hook()
;;   (setq standard-indent 2)
;;   (setq tab-width 2))

;; ;; (add-hook 'web-mode-hook #'rainbow-mode)
;; ;; (add-hook 'web-mode-hook #'flymake-mode)
;; (add-hook 'web-mode-hook #'init-web-mode-hook)


;; only needed because of php-mode
;; (require 'company)
;; (require 'company-php)

;; disable auto display after delay. triggering completion with key
;; strokes works better for me.
;; (setq company-idle-delay nil)


;; --- php mode
;; (require 'php-mode)
;; (require 'php-local-manual)

;; download from https://www.php.net/download-docs.php
;; (setq php-manual-path "/usr/share/doc/php-manual/en/html")
;; (setq php-search-documentation-function #'php-local-manual-search)

;; (defun init-php-mode()
;;   (company-mode t)
;;   ;; enable ElDoc support (optional)
;;   (ac-php-core-eldoc-setup)
;;   (set (make-local-variable 'company-backends)
;;        '((company-ac-php-backend company-dabbrev-code)
;;          company-capf company-files))

;;   ;; remember to mark project root for ac-php by running:
;;   ;; touch .ac-php-conf.json

;;   ;; Jump to definition (optional)
;;   (define-key php-mode-map (kbd "M-.")
;;               'ac-php-find-symbol-at-point)

;;   ;; Return back (optional)
;;   (define-key php-mode-map (kbd "M-,")
;;               'ac-php-location-stack-back))

;; ;; not sure I want camel case navigation in php
;; ;; (add-hook 'php-mode-hook #'subword-mode)
;; (add-hook 'php-mode-hook #'flymake-mode)
;; (add-hook 'php-mode-hook #'init-php-mode)

;; (define-key php-mode-map (kbd "M-<tab>") #'company-complete)

(require 'sgml-mode)

;;; this seems to need being enforced even though other-window key
;;; might be set globally. not sure why, but it works. if not set, a
;;; face menu gets shown in minibuffer.
(define-key html-mode-map my/other-window-kbd #'my/other-window)
(define-key sgml-mode-map my/other-window-kbd #'my/other-window)


;; --- js/typescript mode
(require 'js)
(require 'typescript-mode)

;; Disable js2 mode's syntax error highlighting by default
;; (setq-default js2-mode-show-parse-errors nil
;;               js2-mode-show-strict-warnings nil)


(defun my/js-mode-hook()
  ;; no not start eglot when remote. often starts annoying LSP
  ;; backends that wants multiple tramp connections.
  (if (not (file-remote-p default-directory))
      (eglot-ensure)))
;; (add-hook 'js2-mode-hook #'my/js2-mode-hook)
(add-hook 'js-mode-hook #'electric-indent-local-mode)
(add-hook 'js-mode-hook #'electric-pair-local-mode)

;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))


;;; --- HOCON files
(require 'hcl-mode)

(add-to-list 'auto-mode-alist '("\\.hocon\\'" . hcl-mode))
(add-to-list 'auto-mode-alist '("\\jicofo.conf\\'" . hcl-mode))
(add-to-list 'auto-mode-alist '("\\jvb.conf\\'" . hcl-mode))



(require 'ellama)


;; --- markdown
(require 'markdown-mode)
(require 'markdown-preview-mode)
(require 'flymake-markdownlint nil t)

;; https://github.com/fletcher/MultiMarkdown-6 should be used instead
;; of perl version
(setq markdown-command "multimarkdown")


(defun init-markdown-mode()
  ;; from `markdown-mode' and set to `major-mode' when used. hence why
  ;; use buffer name instead
  (when (not (string-prefix-p "*ellama" (buffer-name)))
    (if (fboundp 'flymake-markdownlint-setup)
        (flymake-markdownlint-setup))
    (flymake-mode t)))

(add-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'markdown-mode-hook #'yas-minor-mode)
(add-hook 'markdown-mode-hook #'init-markdown-mode)
(define-key markdown-mode-map (kbd "C-M-i") #'completion-at-point)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; use github markdown for readmes
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


;; --- dockerfile
(require 'dockerfile-mode)
;; (require 'dockerfile-ts-mode)

(add-hook 'dockerfile-mode-hook #'eglot-ensure)
;; (add-hook 'dockerfile-ts-mode-hook #'eglot-ensure)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(define-key dockerfile-mode-map my/format-kbd #'eglot-format)
;; (define-key dockerfile-ts-mode-map my/format-kbd #'eglot-format)


;; --- nginx mode
(require 'nginx-mode)

(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))
(add-to-list 'auto-mode-alist '("nginx.*\\.conf" . nginx-mode))


;; --- haproxy
(require 'haproxy-mode)


;; --- crontab mode
(require 'crontab-mode)

(add-to-list 'auto-mode-alist '("\\.?cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("/var/cron/tabs/'" . crontab-mode))


;; --- ssh-config-mode
;; convenience
(require 'ssh-config-mode)

(add-to-list 'auto-mode-alist '("/\\.ssh/config\\'" . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/sshd?_config\\'" . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/knownhosts\\'" . ssh-known-hosts-mode))
(add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
(add-hook 'ssh-config-mode-hook #'turn-on-font-lock)



;; --- systemd mode
(require 'systemd)



;; --- plantuml mode
(require 'plantuml-mode)
(require 'flymake-plantuml nil t)


(setq plantuml-default-exec-mode 'jar)
;;; performs better
(setq plantuml-jar-args '("-tpng"))
;;; if not set, plantuml-mode seems to default to text making
;;; plantuml.jar output being shown in a fundamental-mode buffer
(setq plantuml-output-type "png")
;;; setting this to remove default "--illegal-access=deny" from
;;; args. this garbles plantuml-mode preview output with a warning
;;; when running jdk 17 or newer
(setq plantuml-java-args (list "-Djava.awt.headless=true" "-jar"))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(if (fboundp 'flymake-plantuml-setup)
    (add-hook 'plantuml-mode-hook 'flymake-plantuml-setup))

(add-hook 'plantuml-mode-hook 'flymake-mode)

(define-key plantuml-mode-map my/complete-symbol-kbd #'plantuml-complete-symbol)

;; --- monitrc mode
(require 'monitrc-mode nil t)


;; --- fluent-bit-config mode
(require 'fluent-bit-config-mode nil t)

;; --- csv mode
(require 'csv-mode)

(setq csv-separators '("," ";" "|" " "))

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))



;; --- meson mode
(require 'meson-mode)


;; --- cmake mode
(require 'cmake-mode)
(require 'cmake-font-lock)

(add-hook 'cmake-mode-hook #'eglot-ensure)

;; --- sql mode
(require 'sqlformat)
(require 'sql)

;; redefine key
(define-key sql-mode-map my/format-kbd 'sqlformat-buffer)

;; sql-mode pretty much requires your psql to be uncustomised from stock settings
(add-to-list 'sql-postgres-options "--no-psqlrc")

(setq-default sql-input-ring-file-name (locate-user-emacs-file ".sqli_history"))

(load-file (expand-file-name "init-sql.el" user-emacs-directory))
(require 'init-sql)



(defun sanityinc/fix-postgres-prompt-regexp ()
  "Work around https://debbugs.gnu.org/cgi/bugreport.cgi?bug=22596.
Fix for the above hasn't been released as of Emacs 25.2."
  (when (eq sql-product 'postgres)
    (setq-local sql-prompt-regexp "^[[:alnum:]_]*=[#>] ")
    (setq-local sql-prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] ")))

(add-hook 'sql-interactive-mode-hook #'sanityinc/fix-postgres-prompt-regexp)


;; --- latex auctex
(require 'tex)

;; encouraged by docs to be able to extract information from the
;; buffers, although can be slow.
(setq TeX-auto-save t)
(setq TeX-parse-self t)


(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server nil)
;; store auxiliary files to tmp dir as they are usually of no interests.
;; PDF's shouldn't be moved here though. fix that elsewhere.
(setq TeX-output-dir "/tmp")

(add-hook 'TeX-mode-hook #'auto-fill-mode)
(add-hook 'TeX-mode-hook #'yas-minor-mode)
(add-hook 'TeX-mode-hook #'eglot-ensure)

;; Enable reference mangment
(add-hook 'LaTeX-mode-map #'reftex-mode)


;; --- pass
(require 'password-store)
(require 'password-store-otp)


;; --- erc
(require 'erc)
(require 'erc-track)
(require 'erc-stamp)
(require 'erc-fill)
(require 'erc-log)

(load-file (expand-file-name "init-erc.el" user-emacs-directory))
(require 'init-erc)

(defun my/erc-channel-users (&rest ignore)
  "Display how many users (and ops) the current channel has."
  (let ((users 0) (ops 0))
    (if (not (hash-table-p erc-channel-users))
        ""
      (maphash (lambda (k _v)
                 (cl-incf users)
                 (when (erc-channel-user-op-p k)
                   (cl-incf ops)))
               erc-channel-users)
      (pcase (cons (= 0 ops) (= 0 users))
        ('(t . t  ) "")
        ('(t . nil) (format "[%s] " users))
        (_          (format "[%s (@%s)] " users ops))))))


(defun my/erc-ignore-highlight (msg)
  "Don't highlight me when these things happen."
  (let ((message (s-trim-left msg))
        (channel (or (erc-default-target) "")))
    (--any? (s-prefix? it message)
            `("*** Users on"
              "*** Your new nickname is"
              "*** Welcome to the"
              ,(concat "*** " channel ": topic set by")))))


(defun my/erc-log-matches (match-type nickuserhost message)
  "Log matches to extra buffer, unless they are annoying."
  (unless (my/erc-ignore-highlight message)
    (erc-log-matches match-type nickuserhost message)))


(defun my/beep-on-match (match-type _nickuserhost message)
  "Beep and mark the frame as urgent on highlight."
  (let ((visible-bell nil))
    (unless (my/erc-ignore-highlight message)
      ;; (my/mark-emacs-urgent)
      (erc-beep-on-match match-type _nickuserhost message))))


(setq erc-track-switch-direction 'importance)
;; always log highlights
(setq erc-log-matches-flag t)
;; beep on all highlights
(setq erc-beep-match-types '(current-nick keyword))
(setq erc-keyword-highlight-type 'message)
(setq erc-prompt (lambda () (format "%s%s ⟩" (my/erc-channel-users) (buffer-name))))
;;; hide stuff
(setq erc-track-exclude-types '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353"))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
;;; always show timestamp
(setq erc-timestamp-only-if-changed-flag nil)
(setq erc-timestamp-format "[%Y-%m-%d %H:%M]")
(setq erc-insert-timestamp-function #'erc-insert-timestamp-left)
;;; filling
(setq erc-fill-column 190)
(setq erc-fill-function #'erc-fill-static) ; align nicknames
(setq erc-fill-static-center 20)

;;;; Logging
(setq erc-log-channels-directory (expand-file-name "erc-logs" user-emacs-directory))
(setq erc-log-write-after-insert t)
(setq erc-log-write-after-send t)
(setq erc-save-queries-on-quit t)

(add-hook 'erc-text-matched-hook #'my/erc-log-matches)
(add-hook 'erc-text-matched-hook #'my/beep-on-match)


;;; --- eshell
(require 'em-banner)
(require 'em-tramp)                   ; for `sudo'; see aliases
(require 'em-hist)
(require 'em-term)
(require 'em-smart)
(require 'eshell-toggle)



(setq eshell-banner-message "")
(setq eshell-history-size 100000)
(setq eshell-hist-ignoredups t)
(setq eshell-destroy-buffer-when-process-dies t)

(setq eshell-toggle-size-fraction 3)
(setq eshell-toggle-init-function (lambda (_dir) (project-eshell)))

(global-set-key (kbd "M-`") #'eshell-toggle)



;; ensure eshell and friends behaves well
(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")
(setenv "PINENTRY_USER_DATA" nil)
(setenv "GPG_TTY" nil)
(setenv "TERM" "dumb")



;; to aid eshell in case terminal programs demands a TUI
;; (require 'eat)


;; ;; for `eat-eshell-mode'.
;; (add-hook 'eshell-load-hook #'eat-eshell-mode)

;; for `eat-eshell-visual-command-mode'.



(require 'package-lint)

;; --- emms
;; multimedia
;; (eval-and-compile
;;   (require 'emms nil t)
;;   (require 'emms-browser nil t)
;;   (require 'emms-player-mpd nil t)
;;   (require 'emms-setup nil t)
;;   (require 'emms-source-file nil t)
;;   (require 'emms-info-metaflac nil t)
;;   (require 'emms-info-exiftool nil t)
;;   (require 'emms-info-tinytag nil t)

;;   (when (display-graphic-p)

;;     (emms-all)

;;     (setq emms-source-file-default-directory (expand-file-name "~/audio/"))
;;     (setq emms-player-mpd-server-port nil)
;;     (setq emms-browser-thumbnail-small-size 64)
;;     (setq emms-browser-thumbnail-medium-size 128)
;;     (setq emms-source-file-directory-tree-function #'emms-source-file-directory-tree-find)
;;     (setq emms-source-playlist-default-format 'm3u)
;;     (setq emms-info-functions '(emms-info-exiftool))
;;     (setq emms-player-list '(emms-player-mpv))))

;; --- udev mode
(require 'udev-mode)


;;; vundo
(require 'vundo)

(when (display-graphic-p)
  (setq vundo-glyph-alist vundo-unicode-symbols))


;;; --- elfeed
(require 'elfeed)

(defun elfeed-youtube-dl (&optional use-generic-p)
  "Youtube-DL link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (dolist (entry entries)
      (elfeed-untag entry 'unread)
      (let ((link (elfeed-entry-link entry))
            (default-directory "~/dwl"))
        (when link
          (async-shell-command (format "yt-dlp %s" link)))))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))


(setq-default elfeed-search-filter "@2-week-ago +unread +first")

(load-file (expand-file-name "init-elfeed.el" user-emacs-directory))
(require 'init-elfeed)



;; (setq elfeed-show-entry-switch #'pop-to-buffer)
(setq elfeed-enclosure-default-dir (expand-file-name "~/dwl/"))
(setq elfeed-sort-order 'ascending)
(setq elfeed-search-clipboard-type 'CLIPBOARD)
(setq elfeed-search-title-max-width 100)
(setq elfeed-search-title-min-width 30)
(setq elfeed-search-trailing-width 25)
(setq elfeed-show-truncate-long-urls t)
(setq elfeed-show-unique-buffers nil)
(setq elfeed-search-date-format '("%F %R" 16 :left))
(setq elfeed-show-entry-switch #'pop-to-buffer)
(setq elfeed-show-entry-delete #'kill-buffer-and-window)

;; cursor in search buffer should follow current showing entry
(setq elfeed-search-remain-on-entry t)


;; Mark all YouTube entries
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(video youtube)))

;; Entries older than 2 weeks are marked as read
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread))


(defface important-elfeed-entry
  '((t :foreground "#f77"))
  "Marks an important Elfeed entry.")

(push '(important important-elfeed-entry)
      elfeed-search-face-alist)

(defun my/elfeed-quit()
  (interactive)
  (if (and (derived-mode-p 'elfeed-search-mode))
      (progn
        (elfeed-kill-buffer)
        (delete-frame (selected-frame)))
    (progn
      (elfeed-kill-buffer)
      (delete-window))))


(defun init-elfeed-search-mode()
  ;; annoying to have to move cursor to top before scrolling
  (setq-local scroll-margin 4))

(add-hook 'elfeed-search-mode-hook #'init-elfeed-search-mode)


(defun my/elfeed-filter-first()
  (interactive)
  (elfeed-search-set-filter "@2-week-ago +unread +first"))

(defun my/elfeed-filter-second()
  (interactive)
  (elfeed-search-set-filter "@2-week-ago +unread +second"))


(define-key elfeed-show-mode-map (kbd "w") #'elfeed-show-yank)
(define-key elfeed-show-mode-map (kbd "q") #'my/elfeed-quit)
(define-key elfeed-search-mode-map (kbd "1") #'my/elfeed-filter-first)
(define-key elfeed-search-mode-map (kbd "2") #'my/elfeed-filter-second)
(define-key elfeed-search-mode-map (kbd "q") #'my/elfeed-quit)
(define-key elfeed-search-mode-map (kbd "g") #'elfeed-update)
(define-key elfeed-search-mode-map (kbd "G") #'elfeed-search-update--force)
(define-key elfeed-search-mode-map (kbd "d") #'elfeed-youtube-dl)




(require 'edit-server)

(when (getenv "XDG_SESSION_DESKTOP")
  (setq edit-server-url-major-mode-alist '(("reddit\\.com" . markdown-mode)
                                           ("stackexchange\\.com" . markdown-mode)
                                           ("github\\.com" . gfm-mode)))
  ;; (window-system . x) seems to break on pgtk builds
  (setq edit-server-new-frame-alist '((name . "Edit with Emacs FRAME")
                                      (top . 200)
                                      (left . 200)
                                      (width . 80)
                                      (height . 25)
                                      (minibuffer . t)
                                      (menu-bar-lines . t)))

  (add-hook 'edit-server-start-hook #'jinx-mode)
  (unless (server-running-p)
    (edit-server-start)))


(defun ile-elpa ()
  (interactive)
  (byte-recompile-directory (expand-file-name "elpa" user-emacs-directory) 0 t))


(require 'dns-mode)

(add-to-list 'auto-mode-alist '("\\.reverse\\'" . zone-mode))
(add-to-list 'auto-mode-alist '("\\.forward\\'" . zone-mode))
(add-to-list 'auto-mode-alist '("\\.zone\\'" . zone-mode))



;;; syslog
(require 'syslog-mode)

(add-to-list 'auto-mode-alist '(".*/syslog*" . syslog-mode))
(add-to-list 'auto-mode-alist '("/log/syslog" . syslog-mode))
(add-to-list 'auto-mode-alist '("/log/user" . syslog-mode))
(add-to-list 'auto-mode-alist '("/log/daemon" . syslog-mode))
(add-to-list 'auto-mode-alist '("/log/local" . syslog-mode))


(require 'i3wm-config-mode)

(add-to-list 'auto-mode-alist '(".*/i3/config" . i3wm-config-mode))
(add-to-list 'auto-mode-alist '(".*/i3/.*\.config" . i3wm-config-mode))


(require 'ledger-mode)
(require 'ledger-flymake)


(add-hook 'ledger-mode-hook #'ledger-flymake-enable)


(require 'ascii-table)


(require 'caddyfile-mode)

(defun my/caddyfile-hook ()
  (setq-local tab-width 4)  ;; Default: 8
  (setq-local indent-tabs-mode nil))  ;; Default: t


(defun caddy-format-buffer ()
  "Run caddy formatting on the current region or buffer."
  (interactive)

  (let ((current-line (line-number-at-pos))
        (start (point-min)))
    (shell-command-on-region start (point-max) "caddy fmt -" nil t nil t)
    (goto-char start)
    (forward-line (1- current-line))))

(define-key caddyfile-mode-map my/format-kbd #'caddy-format-buffer)

(add-hook 'caddyfile-mode-hook #'my/caddyfile-hook)



(require 'nftables-mode)

(when (file-exists-p emacs-host-custom)
  (load-file emacs-host-custom))
(when (file-exists-p emacs-host-local)
  (load-file emacs-host-local))


(when (display-graphic-p)
  (setq epg-pinentry-mode 'loopback)

  (require 'server)
  (unless (server-running-p)
    (server-start))
  ;; run mu4e in the background
  (let ((muhome (getenv "MUHOME")))
    (if (and (file-directory-p (expand-file-name "~/.mail"))
             muhome
             (file-directory-p (expand-file-name muhome)))
        (with-eval-after-load 'mu4e-icalendar
          ;; no need to start mu4e in background
          (if (and (> mu4e-major-version 0) (> mu4e-minor-version 11))
              (gnus-icalendar-setup)
            (mu4e-icalendar-setup)))
      (warn "mu4e not started. data directories not initialized or environment variables missing"))))


(require 'rfc-mode)

(setq rfc-mode-directory (expand-file-name "/var/db/rfc/"))


;; --- desktop mode
;; conveniently save buffers and window setup between sessions
(require 'desktop)

;; eager restore seems to not always work. disable for now.
(setq desktop-restore-eager nil)
;; sometimes we have debug frames with separate window layout. might
;; as well restore them.
(setq desktop-restore-frames t)
;; needed when loading emacs as daemon, and, for now, generally not
;; a problem loading locked desktops
(setq desktop-load-locked-desktop t)
;; don't care
(setq desktop-missing-file-warning nil)
(setq desktop-save 'ask-if-new)
;;; `flymake-mode' acts weird when started from `desktop-mode' list of
;;; saved minor modes. ignore the mode for now, and let hooks start
;;; `flymake-mode' manually
(add-to-list 'desktop-minor-mode-table '(flymake-mode nil))


;; make sure to have one white space char after M-j
(defadvice comment-indent-new-line (after just-one-space-after-comment activate)
  "Make sure that `comment-indent-new-line' does not insert extra spaces."
  (when (and (looking-back comment-start 6)
             (not (looking-back " " 6)))
    (just-one-space)))



(require 'openhab-mode nil t)
(require 'openhab-persistence-mode nil t)
(require 'openhab-item-mode nil t)
(require 'openhab-rule-mode nil t)
(require 'openhab-sitemap-mode nil t)
(require 'openhab-thing-mode nil t)


;; --- TOML
;; (require 'toml-ts-mode)

;; (define-key toml-ts-mode-map my/format-kbd #'eglot-format-buffer)


;; (add-hook 'toml-ts-mode-hook #'eglot-ensure)


;; --- verb
(require 'verb)

(setq verb-base-headers '(("User-Agent" . "curl/8.8.0")))
;; (require 'restclient)

;; (add-to-list 'auto-mode-alist '("\\.rest*\\'" . restclient-mode))


;; --- ada mode

(require 'ada-mode)

(setq-default ada-face-backend 'none)
(setq-default ada-indent-backend 'none)
(setq-default ada-xref-backend 'eglot)

;;; --- tree-sitter

(require 'treesit)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        ;; (make "https://github.com/alemuller/tree-sitter-make")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        ;; (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")))

;; yaml-ts-mode is a bit more annoying or inferior than yaml-mode
;; (yaml-mode . yaml-ts-mode)
(setq major-mode-remap-alist
      '(
        (python-mode . python-ts-mode)
        (bash-mode . bash-ts-mode)
        (go-mode . go-ts-mode)
        (css-mode . css-ts-mode)
        (js2-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        ))


(defun my/treesit-install-language-grammar-all()
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))




;; --- perl
(require 'cperl-mode)

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(add-hook 'cperl-mode-hook #'eglot-ensure)
(add-hook 'perl-mode-hook #'eglot-ensure)


;; --- gnuplot
(require 'gnuplot)

(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))


(require 'etc-sudoers-mode)

(setq display-buffer-alist
      `(
        ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
         nil
         (window-parameters (mode-line-format . none)))
        ("\\*eshell\\*"
         (display-buffer-reuse-mode-window display-buffer-in-previous-window)
         )
        ;; place occur in a 'smaller' buffer below window. It often
        ;; takes up too much place in another window
        ("\\*Occur\\*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (dedicated . t))
        ("\\*PLANTUML.*"
         (display-buffer-no-window)
         (dedicated . t))
        ;; ("\\*Man\\.*"
        ;;  (display-buffer-reuse-mode-window display-buffer-in-direction)
        ;;  ;; `mode' is needed for reuse-mode-window to work
        ;;  (mode . Man-mode)
        ;;  (direction . leftmost)
        ;;  (window-width . 83)
        ;;  ;; strictly not needed but shouldn't hurt
        ;;  (dedicated . t)
        ;;  ;; needed if window switch happens withing the same
        ;;  ;; window. like hitting 'm' while in a Man-mode window
        ;;  (inhibit-same-window . nil)
        ;;  )
        ;; no window
        ("\\`\\*Async Shell Command\\*\\'"
         (display-buffer-no-window))
        ;; top side window
        ("\\*Messages.*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . bottom)
         (slot . 1))
        ("\\*Org Select\\*"
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom)
         (slot . 0))
        ("\\(magit: .+\\|magit-log.+\\|magit-revision.+\\)"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (mode . magit-mode))
        ("\\*\\(Calendar\\|Bookmark Annotation\\|Buffer List\\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.4))
        ("\\*CAPTURE\\.*"
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom)
         (slot . 0))
        ("\\*elfeed-entry\\.*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (inhibit-switch-frame . t)
         (window-height . 0.7))
        ("\\*\\(Flymake diagnostics\\|Package-Lint\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.20)
         (side . bottom)
         (slot . -1))
        ;; PLANTUML Preview buffers seems to be removed before
        ;; refreshing preview, so reuse-mode-window probably cannot be
        ;; used in this case
        ("\\*\\(Occur\\|PLANTUML Preview\\).*"
         ;; (display-buffer-reuse-mode-window display-buffer-use-some-window )
         (display-buffer-reuse-mode-window)
         (inhibit-same-window . nil)
         ;; (inhibit-switch-frame . t)
         ;; (direction . leftmost)
         )
        ;; Apropos\\| removed
        ("\\*\\(eldoc\\).*"
         ;; Help\\| moved out for now
         (display-buffer-in-side-window)
         (side . bottom)
         ;; (fail)
         )
        ((or . ((derived-mode . Man-mode)
                (derived-mode . woman-mode)
                "\\*\\(Man\\|woman\\).*"))
         (display-buffer-same-window))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Compilation\\|Messages\\|Flymake log\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.20)
         (side . bottom)
         (slot . 0))
        ))


;; unset keyboard macro recordings. never using it.
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
(global-unset-key (kbd "C-x ("))
(global-unset-key (kbd "C-x )"))
(global-unset-key (kbd "C-x C-k RET"))
(global-unset-key (kbd "C-x C-k n"))
(global-unset-key (kbd "C-x C-k r"))
(global-unset-key (kbd "C-x e"))

(require 'olivetti nil t)
;; (require 'spacious-padding nil t)
;; (setq-default spacious-padding-widths '( :internal-border-width 7
;;                                          :header-line-width 4
;;                                          :mode-line-width 6
;;                                          :tab-width 4
;;                                          :right-divider-width 30
;;                                          :scroll-bar-width 8))

(when (display-graphic-p)
  ;; --- pdf-tools
  (require 'pdf-tools nil t)
  (if (fboundp 'pdf-tools-install)
      (pdf-tools-install t t nil nil))

  ;; by enabling tab-bar-mode early workaround rendering "bug" where
  ;; tab bar is not drawn until frame is resized
  ;; (tab-bar-mode t)

  ;; olivetti fails on emacs-nox with issues regarding mouse-wheel-*
  ;; symbols

  (setq olivetti-body-width 1)
  (setq olivetti-minimum-body-width 180)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  (setq olivetti-lighter "")
  (add-hook 'prog-mode-hook #'olivetti-mode)
  (add-hook 'conf-mode-hook #'olivetti-mode)
  (add-hook 'shell-mode-hook #'olivetti-mode)
  (add-hook 'eshell-mode-hook #'olivetti-mode)
  (add-hook 'ssh-config-mode-hook #'olivetti-mode)
  (add-hook 'outline-mode-hook #'olivetti-mode)
  (add-hook 'org-mode-hook #'olivetti-mode)
  (add-hook 'markdown-mode-hook #'olivetti-mode)
  (add-hook 'mu4e-view-mode-hook #'olivetti-mode)
  (add-hook 'mu4e-compose-mode-hook #'olivetti-mode)
  (add-hook 'elfeed-show-mode-hook #'olivetti-mode)
  (add-hook 'elfeed-search-mode-hook #'olivetti-mode)
  ;; disabled because minibuffer ends up flicker and return to
  ;; non-olivetti layout. it works at the begging of emacs but
  ;; something distorts the layout after some time.
  ;;
  ;; (add-hook 'minibuffer-setup-hook #'olivetti-mode)
  (add-hook 'toml-ts-mode-hook #'olivetti-mode)
  (add-hook 'yaml-ts-mode-hook #'olivetti-mode)
  (add-hook 'rfc-mode-hook #'olivetti-mode)
  (add-hook 'ibuffer-mode-hook #'olivetti-mode)
  (add-hook 'Man-mode-hook #'olivetti-mode)
  (add-hook 'ledger-mode-hook #'olivetti-mode)
  (add-hook 'Info-mode-hook #'olivetti-mode)
  (add-hook 'nxml-mode-hook #'olivetti-mode)
  (add-hook 'latex-mode-hook #'olivetti-mode)
  (add-hook 'gnuplot-mode-hook #'olivetti-mode)
  ;; (spacious-padding-mode t)
  )


(require 'keycast nil t)
(setq keycast-mode-line-remove-tail-elements nil)
(setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)


(require 'transient)

(require 'casual-isearch)

(define-key isearch-mode-map (kbd "C-o") #'casual-isearch-tmenu)


(defun my/shrink-window (arg)
  (interactive "p")
  (if (not (shrink-window arg t))
      (shrink-window arg nil)))
(defun my/enlarge-window (arg)
  (interactive "p")
  (if (not (enlarge-window arg t))
      (enlarge-window arg nil)))


(transient-define-prefix my/transient-window ()
  "Window navigation transient"
  :transient-suffix 'transient--do-stay
  [["Zoom"
    ("g" "in" text-scale-increase "in")
    ("l" "out" text-scale-decrease "out")]
   ["Resize"
    ("[" "shrink" my/shrink-window "8")
    ("]" "enlarge" my/enlarge-window "8")]
   ["Main"
    ("<f10>" "quit" transient-quit-all)]
   ])


(defun my/split-window-sensibly (&optional window)
  "replacement `split-window-sensibly' function which prefers
vertical splits"
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             (with-selected-window window
               (split-window-below))))))

(setq split-window-preferred-function #'my/split-window-sensibly)


(when (getenv "IS_GENTOO")
  (require 'flymake-pkgcheck nil t)
  (if (fboundp 'flymake-pkgcheck-setup)
      (add-hook 'ebuild-mode-hook #'flymake-pkgcheck-setup)))

(global-set-key (kbd "<f10>") 'my/transient-window)


;;; init.el ends here
