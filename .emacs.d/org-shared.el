;;; package --- org shared config file
;;; Commentary:
;;; Code:

(require 'org)
(require 'ox-icalendar)


(setq org-icalendar-alarm-time 30)
(setq org-icalendar-include-todo nil)
(setq org-icalendar-use-deadline '(event-if-not-todo event-if-todo-not-done todo-due))
(setq org-icalendar-use-scheduled '(event-if-not-todo event-if-todo-not-done todo-start))
(setq org-icalendar-with-timestamps 'active)
(setq org-icalendar-combined-agenda-file (expand-file-name "combined-agenda.ics" org-directory))

(provide 'org-shared)
;;; org-shared.el ends here
