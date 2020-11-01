;;; package --- main init file
;;; Commentary:
;;; Code:

(require 'ispell)
(require 'nxml-mode)

(defun compile-pkg (&optional command startdir)
  "Compile a package with COMMAND.
Moving up to the parent directory containing configure.ac, if it
exists.  Start in STARTDIR if defined, else start in the current directory."
  (interactive)

  (let ((dirname)
	(dir-buffer nil))
    (setq startdir (expand-file-name (if startdir startdir ".")))
    (setq command  (if command command compile-command))

    (setq dirname (upward-find-file "configure.ac" startdir))
    (setq dirname (if dirname dirname (upward-find-file "Makefile" startdir)))

    (setq dirname (if dirname dirname (expand-file-name ".")))
    ; We've now worked out where to start. Now we need to worry about
    ; calling compile in the right directory
    (save-excursion
      (setq dir-buffer (find-file-noselect dirname))
      (set-buffer dir-buffer)
      (compile command)
      (kill-buffer dir-buffer))))

(defun upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename FILENAME.
If we manage to find it, return the containing directory.  Else if we get to the
toplevel directory and still can't find it, return nil.  Start at STARTDIR or .  if STARTDIR not given."

  (let ((dirname (expand-file-name
		  (if startdir startdir ".")))
	(found nil) ; found is set as a flag to leave loop if we find it
	(top nil))  ; top is set when we get
		    ; to / so that we only check it once

    ; While we've neither been at the top last time nor have we found
    ; the file.
    (while (not (or found top))
      ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
	  (setq top t))

      ; Check for the file
      (if (file-exists-p (expand-file-name filename dirname))
	  (setq found t)
	; If not, move up a directory
	(setq dirname (expand-file-name ".." dirname))))
    ; return statement
    (if found dirname nil)))

(defun notify-compilation-result(buffer string)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful"
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      ;; (switch-to-prev-buffer (get-buffer-window
                      ;; buf) 'kill))
                      (delete-windows-on buf))
                    buffer))
  ;; old code
  ;; (if (string-match "^finished" msg)
  ;;     (delete-windows-on buffer)
  ;;   )

  )


(defun nxml-pretty-format ()
  "."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region 0 (count-lines (point-min) (point-max))))
  )


(defun switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "en_US") "da_DK" "en_US")))
    (ispell-change-dictionary change)
    ;; Dont check whole buffer as it can be of several languages, and large
    (if (bound-and-true-p flyspell-mode)
        (flyspell-buffer))
    (message "Dictionary switched from %s to %s" dic change)))

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

(defvar local--package-refreshed)

(defun ensure-package (packagename)
  (unless (package-installed-p packagename)
    (if (not (boundp 'local--package-refreshed))
        (eval (package-refresh-contents)
              (setq local--package-refreshed t)))
    (package-install packagename)
  ))


(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
          (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(provide 'local-common)
;;; local-common.el ends here
