;;; early-init --- Summary
;;; Commentary:

;;; Code:

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿

;; if added emacs seem to recognize foot as a 256 color terminal.
;; use (list-colors-display) to check
(add-to-list 'term-file-aliases '("foot" . "xterm"))

;; nil in order to control package initialization using init.el
(setq package-enable-at-startup nil)

;; maintain a different package dir for each OS. different package
;; managers might include emacs lisp others do not.
(if (string= (getenv "IS_DEBIANIZED") "1")
    (setq-default package-user-dir (locate-user-emacs-file "elpa-debian")))
(if (string= (getenv "IS_GENTOO") "1")
    (setq-default package-user-dir (locate-user-emacs-file "elpa-gentoo")))


(when (native-comp-available-p)
  ;; enable all CPU cores when compiling
  (setq-default native-comp-async-jobs-number 0)
  ;; produce warnings and errors but don't pop the window up
  (setq native-comp-async-report-warnings-errors nil))

;; should be the default for tiling window managers as emacs usually
;; don't have a say in window sizing when changing fonts and emacs
;; trying to preserve the amount of columns
(setq-default frame-inhibit-implied-resize t)

;; Disable GUI elements
(when window-system
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
;; should only test this on GUI Emacs. function does not exists
;; on emacs-nox
;; (when (fboundp 'toggle-scroll-bar)
;;   ;; remove scroll bars. not needed.
;;   (toggle-scroll-bar nil))
(setq inhibit-splash-screen t)
(setq use-dialog-box t)
(setq use-file-dialog nil)

;; remove
(setq inhibit-startup-echo-area-message user-login-name) ; read the docstring
;; no need start up screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

;; load machine local configuration
(defvar early-init-local (expand-file-name (format "early-init-local-%s.el" (system-name)) user-emacs-directory))
(when (file-exists-p early-init-local)
  (load-file early-init-local))

;;; early-init.el ends here
