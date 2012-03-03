;;; -*- lexical-binding: t -*-

;;; basecamp.el --- Elisp API bindings to Basecamp.

;; Copyright (C) 2012
;; Author: Kirk Kelsey
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
;;
;; This code is based on org-camp.el:
;;
;;  https://github.com/0x4b/org-camp
;;

(require 'url)

(defconst basecamp-version "0.1" "Version of `basecamp'.")

(defgroup basecamp nil
  "Org-mode package for importing todo lists from Basecamp.com."
  :group 'org)

(defcustom basecamp-auth-token ""
  "User authentication token."
  :group 'basecamp
  :type '(string))

(defcustom basecamp-person ""
  "Default person."
  :group 'basecamp
  :type '(string))

(defcustom basecamp-subdomain ""
  "The subdomain that should preceed .basecamphq.com in URLs."
  :group 'basecamp
  :type '(string))

(defun basecamp-url (request)
  "Returns a Basecamp URL build from `basecamp-subdomain' and the
provided `request' string."
  (cond ((= (length basecamp-subdomain) 0)
         (error "The value of `basecamp-subdomain' must be customized"))
        (t
		 (concat "https://" basecamp-subdomain ".basecamphq.com/" request))))

(defvar org-cache (make-hash-table))

;; This could optionally take a "?responsible_party=#{id}"
(defun basecamp-request (item &optional callback data)
  (let ((-callback- callback)
		(cached-val (gethash (intern item) org-cache))
		(-item- item)
		(-data- data))
	(if (and cached-val (not data))
		(if callback
			(funcall -callback- cached-val)
		  cached-val)
	  (let ((url-request-method (if data "POST" "GET"))
			(url-request-data (if data (encode-coding-string data 'utf-8)))
			(url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
			(url-request-coding-system 'utf-8)
			;; (url-https-asynchronous-p nil)
			(url-request-extra-headers
			 `(("Accept" . "application/xml")
			   ("Content-Type" . "application/xml; charset=utf-8")
			   ("Authorization" .
				,(concat "Basic "
						 (base64-encode-string
						  (concat basecamp-auth-token ":X")))))))

		(let ((buffer (url-retrieve-synchronously (basecamp-url (concat item ".xml")))))
		  (when buffer
			(let ((parsed (xml-parse-region 1 (buffer-size buffer) buffer)))
			  (unless -data-
				(puthash (intern -item-) parsed org-cache))
			  (if callback
				  (funcall -callback- parsed)
				parsed))))))))

(defun basecamp-get-projects (&optional callback)
  ""
  (basecamp-request "projects" callback))

(defun basecamp-get-todolist (&optional callback)
  ""
  (basecamp-request "todo_lists" callback))

(defun basecamp-get-todolists-project (project-id &optional callback)
  ""
  (basecamp-request (concat "projects/" project-id "/todo_lists") callback))


(defun basecamp-get-todo-items-todo-list (todo-list-id &optional callback)
  ""
  (basecamp-request (concat "todo_lists/" todo-list-id "/todo_items") callback))

(defun basecamp-get-project-names (projects)
  (basecamp-map-xml-children
   (car projects) 'project '(name id)
   (lambda (xml name id)
	 `(,id . ,name))))

;; This probably exists.
(defun basecamp-reverse-alist (alist)
  (mapcar
   (lambda (el)
	 `(,(cdr el) . ,(car el)))
   alist))

(defun basecamp-xml-child-content (xml name)
  (car (xml-node-children (car (xml-get-children xml name)))))

(defun basecamp-map-xml-children (xml child-name child-map f)
  (mapc
   (lambda (child)
	 (apply f (cons child
					(mapcar (lambda (gchild-name)
							  (basecamp-xml-child-content child gchild-name))
							child-map))))
   (xml-get-children xml child-name)))


(defun basecamp-reverse-cread (prompt alist)
  (let* ((alist-rev (basecamp-reverse-alist alist))
		 (val (ido-completing-read prompt alist-rev nil t)))
	(cdr (assoc val alist-rev))))

(defun basecamp-clear-cache ()
  (interactive)
  (setq org-cache (clrhash org-cache)))

(defun basecamp-get-me (callback)
  (basecamp-request "me" callback))

(defun basecamp-get-me-id ()
  (basecamp-get-me
   (lambda (me)
	 (basecamp-xml-child-content (car me) 'id))))

(defun basecamp-ask-hours (&optional initial-contents)
  (let ((hours (read-from-minibuffer "Time: " initial-contents)))
	;; Check that hours is either a float or on minutes format
	;; (:minutes), return nil otherwise.
	(when (or (string-match ":[[:digit:]]+" hours)
			  (not (eq (string-to-number hours) 0)))
	  hours)))

(defun basecamp-ask-date (&optional initial-contents)
  (let* ((dflt (format-time-string "%Y-%m-%d"))
		 (val (read-from-minibuffer (concat "Date (" dflt "): ") initial-contents)))
	(if (string= val "") dflt val)))

(defun basecamp-ask-description (&optional initial-contents)
  (read-from-minibuffer "Description: " initial-contents))

;;;###autoload
(defun basecamp-log-time-todo (todo-id person date hours description)
  (interactive
   `(,(basecamp-get-projects-todolist-todo)
	 ,(basecamp-get-me-id)
	 ,(basecamp-ask-date)
	 ,(basecamp-ask-hours)
	 ,(basecamp-ask-description)))


  (basecamp-request
   (concat "/todo_items/" todo-id "/time_entries")
   (lambda (xml) (message "Logged!"))
   (concat
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	"<time-entry>"
	"<person-id>" person  "</person-id>"
	"<date>" date "</date>"
	"<hours>" hours "</hours>"
	"<description>" description "</description>"
	"</time-entry>")))

;;;###autoload
(defun basecamp-get-time-entries-project (project-id)
  ""
  (interactive
   `(,(basecamp-get-projects
	   (lambda (projects)
		 (basecamp-reverse-cread "Project: " (basecamp-get-project-names projects))))))

  (basecamp-request
   (concat "projects/" project-id "/time_entries")
   (lambda (xml)
	 (message xml))))

;;;###autoload

;; TODO: Currently shows projects that are arcvhiec, and proejcts
;; without lists. It also shows lists without todos. Also shows all
;; todo-items, regardless of assignment and completion status.
(defun basecamp-get-projects-todolist-todo ()
  (interactive)
  (basecamp-get-projects
   (lambda (projects)
	 (let* ((todo-list-id-name nil)
			(todo-items-id-name nil)

			(proj-name-id (basecamp-get-project-names projects))
			(project-id (basecamp-reverse-cread "Project: " proj-name-id)))

	   (basecamp-get-todolists-project
		project-id
		(lambda (todolists)

		  (basecamp-map-xml-children
		   (car todolists) 'todo-list '(name id)
		   (lambda (xml todo-list-name todo-list-id)
			 (setq todo-list-id-name
				   (cons `(,todo-list-id . ,todo-list-name)
						 todo-list-id-name))))

		  (let* ((todo-list-id (basecamp-reverse-cread "Todo-list: " todo-list-id-name)))
			(basecamp-get-todo-items-todo-list
			 todo-list-id
			 (lambda (todo-items)
			   (basecamp-map-xml-children
				(car todo-items) 'todo-item '(content id)
				(lambda (xml todo-item-content todo-item-id)
				  (setq todo-items-id-name
						(cons `(,todo-item-id . ,todo-item-content)
							  todo-items-id-name)))))))

		  (basecamp-reverse-cread "Todo: " todo-items-id-name)))))))

(provide 'basecamp)

;;; basecamp ends here
