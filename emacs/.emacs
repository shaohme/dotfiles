;;; package --- main init file
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
       '(
         ;; to be compatible with emacs-27
         (lua-mode           . "MELPA")
         ;; to be compatible with
         ;; lsp-mode. flcheck-32 is needed
         (flycheck           . "MELPA")
         )))

(package-initialize)

(defvar emacs-local (expand-file-name "emacs.local.el" user-emacs-directory))
(defvar emacs-secrets (expand-file-name "emacs.secrets.el" user-emacs-directory))

(load (expand-file-name "emacs.common.el" user-emacs-directory))
(when (file-exists-p emacs-local)
  (load emacs-local))
(when (file-exists-p emacs-secrets)
  (load emacs-secrets))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "6ebdb33507c7db94b28d7787f802f38ac8d2b8cd08506797b3af6cdfd80632e0" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(package-selected-packages
   '(moe-theme ample-theme counsel-projectile flycheck zenburn-theme xclip projectile ivy-xref ivy-rich counsel company amx))
 '(safe-local-variable-values
   '((pyvenv-workon . syseventd)
     (pyvenv-workon . inputeventd)
     (pyvenv-workon . pulsemand)
     (pyvenv-workon . scm-utils))))
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
