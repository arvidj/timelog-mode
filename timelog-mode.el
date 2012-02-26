;;; timelog-mode.el --- Log time to Basecamp from Emacs.

;; Copyright (C) 2012
;; Author: Arvid Jakobsson

;; This file is *not* a part of GNU Emacs.

;;; License

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

(require 'basecamp)

(define-derived-mode timelog-mode text-mode
  "Timelog-mode"

  (let ((map timelog-mode-map))
    (define-key map (read-kbd-macro "M-n") 'timelog-next)
    (define-key map (read-kbd-macro "M-p") 'timelog-prev)
    (define-key map (read-kbd-macro "C-c l") 'timelog-log-this))

  (add-hook 'post-command-hook #'timelog-post-command-hook t t))

;; Create the keymap for this mode.
(defvar timelog-mode-map (make-sparse-keymap))

(defun timelog-trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "\\`[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+\\'" s) (setq s (replace-match "" t t s)))
  s)

(defun timelog-next-moment (&optional count)
  (re-search-forward timeclock-moment-regexp nil t count))

(defun timelog-prev-moment (&optional count)
  (re-search-backward timeclock-moment-regexp nil t count))

(defun timelog-diff-in-minutes (t1 t2)
  (/ (time-to-seconds (subtract-time t2 t1)) 60))

(defun arvid-next-log-in (dir)
  (catch 'found
    (while (if (> dir 0)
	       (timelog-next-moment)
	     (timelog-prev-moment))
      (save-excursion
	(goto-char (match-beginning 0))
	(let ((mom (timeclock-read-moment)))
	  (when (string= (car mom) "i")
	    (throw 'found `(,(match-beginning 0) . ,mom))))))))

(defvar timelog-overlay nil)

(defun timelog-next (arg)
  (interactive "p")
  (let ((next-log-in (arvid-next-log-in arg)))
    (when next-log-in
      (let* ((p1 (car next-log-in))
	     (mom1 (cdr next-log-in))
	     (next (save-excursion (goto-char (match-end 0))
				   (if (timelog-next-moment 2)
				       (match-beginning 0)
				     (point-max)))))

	;; Make sure all of the log-item is visible
	(unless (pos-visible-in-window-p next)
	  (scroll-up (- (line-number-at-pos next)
			(line-number-at-pos (point)))))

	))))

(defun timelog-prev (arg)
  (interactive "p")
  (timelog-next (* -1 arg)))

(defun timelog-log-this ()
  ""
  (interactive)
  (let* ((section (timelog-get-current-log))
	 (start (atl-section-start section))
	 (end (atl-section-end section))

	 ;; TODO: Check, if timelog project on format TODO:todo-nr,
	 ;; then we can skip this step.
	 (todo (basecamp-get-projects-todolist-todo))
	 (id (basecamp-get-me-id))

	 (date (basecamp-ask-date (atl-section-date section)))
	 (hours (basecamp-ask-hours (number-to-string
				     (* 0.5 (round (+ 15 (atl-section-minutes section)) 30)))))
	 (description (basecamp-ask-description (atl-section-comment section))))

    (if (save-window-excursion
	  (select-window (split-window-vertically -10))
	  (switch-to-buffer "*LOG-ITEM*")

	  ;; TODO: pretty print todo item.
	  (insert (format "Log %s hours to todo-item %s, date %s\n" hours todo date))
	  (when description (insert (format "With comment: %s\n" description)))

	  (if (y-or-n-p "Log this? ")
	      (basecamp-log-time-todo todo id date hours description)))
	(save-excursion
	  (goto-char end)
	  (insert (format "\n>> %s HOURS LOGGED\n\n" hours))))

    (kill-buffer "*LOG-ITEM*")))

(defstruct atl-section
  start end date minutes comment)

(defun timelog-get-current-log ()
  (save-excursion
    (unless (and (looking-at timeclock-moment-regexp)
		 (string= (match-string 1) "i"))
      (arvid-next-log-in -1))

    (let* ((mom1 (timeclock-read-moment))
	   (p1 (match-beginning 0))

	   (p2 (progn (goto-char (match-end 0))
		      (timelog-next-moment)
		      (goto-char (match-beginning 0))))
	   (mom2 (save-excursion (timeclock-read-moment)))

	   (next (save-excursion (goto-char (match-end 0))
				 (if (timelog-next-moment)
				     (match-beginning 0)
				   (point-max))))

	   (date (format-time-string "%Y-%m-%d" (cadr mom1)))
	   (minutes (timelog-diff-in-minutes (cadr mom1) (cadr mom2)))
	   (desc (save-excursion (if (re-search-forward "^>>" next t)
				     (timelog-trim (buffer-substring (match-end 0) next))
				   nil))))

      (make-atl-section
       :start p1 :end next :date date :minutes minutes :comment desc))))

(defun timelog-highlight-log ()

  (let* ((section (timelog-get-current-log))
	 (start (atl-section-start section))
	 (end (atl-section-end section)))

    (if (not timelog-overlay)
	(progn (setq timelog-overlay (make-overlay start (1- end)))
	       (overlay-put timelog-overlay 'face 'highlight))
      (move-overlay timelog-overlay start (1- end)))))

(defun timelog-post-command-hook ()
  (timelog-highlight-log))


(provide 'timelog-mode)
