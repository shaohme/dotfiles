;;; package --- batch config file
;;; Commentary:
;;; Code:
(defvar emacs-org-shared (expand-file-name "org-shared.el" user-emacs-directory))
(defvar emacs-init-org (expand-file-name "init-org.el" user-emacs-directory))
(defvar emacs-early-init (expand-file-name "early-init.el" user-emacs-directory))

(load-file emacs-early-init)

(package-initialize)

(load-file emacs-org-shared)

(load-file emacs-init-org)

(require 'ox)
(require 'ox-icalendar)

(setq org-export-with-broken-links t)

(defun my-org-export-ics()
  (interactive)
  (org-icalendar-export-agenda-files)
  (org-icalendar-combine-agenda-files))

;;; org-batch.el ends here
