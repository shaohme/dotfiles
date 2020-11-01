;;; flycheck-docker-compose-config.el --- Check Docker Compose files with “docker-compose config” -*- lexical-binding: t; -*-
;;
;; Copyright 2018 Joe Wreschnig
;;
;; Author: Joe Wreschnig
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "24") (flycheck "31") (docker-compose-mode "1"))
;; Package-Version: 0
;; URL: https://gitlab.com/joewreschnig/flycheck-docker-compose-config/
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Check Docker Compose files with “docker-compose config” when using
;; Flycheck in ‘docker-compose-mode’.
;;
;; After installing, to turn it on call
;; ‘flycheck-docker-compose-config-enable’.
;;
;; For information about Docker Compose files, see URL
;; ‘https://docs.docker.com/compose/compose-file/’.  For
;; ‘docker-compose-mode’, see URL
;; ‘https://github.com/meqif/docker-compose-mode’.


;;; Code:

(require 'flycheck)
(require 'yaml-mode)

(defun flycheck-docker-compose-config--goto-path (path)
  "Go to the YAML element specified by a “.”-delimited PATH.

If the full path could not be resolved, go to the last element
which could be found."
  (goto-char 0)
  (condition-case nil
      (let ((prefix "^"))
        (dolist (key (split-string path "\\."))
          (re-search-forward
           (concat "\\(" prefix "\\)\\([\"']?\\)\\(" key "\\)\\>\\2 *:"))
          (setq prefix (concat (match-string 1) " +"))
          (goto-char (match-end 3))))
    (search-failed nil)))

(defun flycheck-docker-compose-config--goto-variable (var)
  "Go to the first usage of the environment variable VAR."
  (goto-char 0)
  (condition-case nil
      (progn
        (re-search-forward
         (format "[^$]\\${?\\(%s\\)\\($\\|[-?:}\" ]\\)" var))
        (goto-char (match-end 1)))
    (search-failed nil)))

(defun flycheck-docker-compose-config--goto-message (err message)
  "Try to go to the location described by MESSAGE from ERR.

Semantic errors from ‘docker-compose’ don’t contain any line numbers,
only a description of the problem."
  (cond
   ((string-match "The \\([^ ]+\\) variable is not set" message)
    (flycheck-docker-compose-config--goto-variable (match-string 1 message))
    (setf (flycheck-error-level err) 'warning))

   ;; Unfortunately, no amount of venture capital can buy good taste.
   ;; https://github.com/docker/compose/issues/5773

   ;; A path containing a problematic key - blame the key:
   ((string-match "\\([^ ]+\\) contains .* '\\([^']+\\)'" message)
    (flycheck-docker-compose-config--goto-path
     (concat (match-string 1 message) "." (match-string 2 message))))

   ;; The same, but different:
   ((string-match "for \\([^ ]+\\): '\\([^']+\\)'" message)
    (flycheck-docker-compose-config--goto-path
     (concat (match-string 1 message) "." (match-string 2 message))))

   ;; A problem with a subkey in a service:
   ((string-match "\"\\([^\"]+\\)\" option in service \"\\([^\"]+\\)\"" message)
    (flycheck-docker-compose-config--goto-path
     (format "services.%s.%s" (match-string 2 message) (match-string 1 message))))

   ;; A path with a problem itself:
   ((string-match "\\([^ ]+\\) contains" message)
    (flycheck-docker-compose-config--goto-path (match-string 1 message)))

   ;; The quoted bit after the filename is the YAML path:
   ((string-match "In file .*, .* '\\([^']+\\)'" message)
    (flycheck-docker-compose-config--goto-path
     (concat " *" (match-string 1 message))))

   ;; But if it didn’t give a file name, then the first word is probably
   ;; the path to the problematic quoted bit:
   ((string-match "^\\([^ ]+\\)\\>.*'\\([^']+\\)'" message)
    (flycheck-docker-compose-config--goto-path
     (concat (match-string 1 message) "." (match-string 2 message))))

   ;; Running out of ideas - anything in quotes is a path?
   ((string-match "\"\\([^\"]+\\)\"" message)
    (flycheck-docker-compose-config--goto-path (match-string 1 message)))
   ((string-match "'\\([^']+\\)'" message)
    (flycheck-docker-compose-config--goto-path (match-string 1 message)))

   ;; Last gasp - is the first word a key, even a deeply-nested one?
   ((string-match "^\\([^ ]+\\)" message)
    (flycheck-docker-compose-config--goto-path
     (concat " *" (match-string 1 message))))))

(defun flycheck-docker-compose-config--error-filter (errors)
  "Fix up the line numbers of each error in ERRORS, if necessary."
  (dolist (err errors)
    (when (and (not (flycheck-error-line err)) (flycheck-error-message err))
      (flycheck-error-with-buffer err
        (save-restriction
          (save-mark-and-excursion
           (widen)
           (flycheck-docker-compose-config--goto-message
            err (flycheck-error-message err))
           (setf (flycheck-error-line err) (line-number-at-pos)
                 (flycheck-error-column err) (current-column)))))))
  errors)

(flycheck-define-checker docker-compose-config
  "Check Docker Compose files with “docker-compose config”.

For information about Docker Compose files, see URL
‘https://docs.docker.com/compose/compose-file/’.

Docker Compose only includes line numbers when encountering a
parsing error.  Other errors are specified inconsistently,
usually by YAML key path or parts of a key path.  Consequently,
line attribution for this checker is unreliable."
  :command ("docker-compose" "--no-ansi" "-f" source "config" "-q")
  :error-patterns
  (;; YAML parse errors are reasonably well-formatted.
   (error line-start (message) "\n"
          "  in \"" (file-name) "\", line " line ", column " column
          line-end)

   ;; The rest is an inconsistent mess, so we’ll try almost anything.
   (ignore line-start "The Compose file ")
   (ignore line-start "Some services ")
   (ignore line-start "You might ")
   (ignore line-start "For more ")
   (error line-start (message (regexp ".+")) line-end))

  :error-filter flycheck-docker-compose-config--error-filter
  :modes docker-compose-mode)

;;;###autoload
(defun flycheck-docker-compose-config-enable ()
  "Enable checking Docker Compose files with “docker-compose config”."
  (add-to-list 'flycheck-checkers 'docker-compose-config))


(provide 'flycheck-docker-compose-config)
;;; flycheck-docker-compose-config.el ends here
