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
(defun basecamp-request (item callback &optional data)
  (let ((-callback- callback)
		(cached-val (gethash (intern item) org-cache))
		(-item- item)
		(-data- data))
	(if (and cached-val (not data))
		(funcall -callback- cached-val))

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
			(funcall -callback- parsed)))))))

(defun basecamp-get-projects (callback)
  ""
  (basecamp-request "projects" callback))

(defun basecamp-get-todolist (callback)
  ""
  (basecamp-request "todo_lists" callback))

(defun basecamp-get-project-names (projects)
  (mapcar
   (lambda (project)
	 (let ((name (car (xml-node-children (car (xml-get-children project 'name)))))
		   (id (car (xml-node-children (car (xml-get-children project 'id))))))
	   `(,id . ,name)))
   (xml-get-children (car projects) 'project)))

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
;; TODO: Currently only shows todos that are not done, and that are
;; assigned to you.
(defun basecamp-get-projects-todolist-todo ()
  (interactive)
  (basecamp-get-projects
   (lambda (projects)
	 ;; Index project-name by project-id
	 (basecamp-get-todolist
	  (lambda (todos)
		(let ((proj-name-id (basecamp-get-project-names projects))
			  (project-todolists (make-hash-table))
			  (todolist-todos (make-hash-table)))

		  ;; Index todo-lists by project id.
		  (basecamp-map-xml-children
		   (car todos) 'todo-list '(project-id name id)
		   (lambda (xml project-id todo-list-name todo-list-id)

			 (puthash (intern project-id)
					  (cons `(,todo-list-id . ,todo-list-name)
							(gethash (intern project-id) project-todolists))
					  project-todolists)

			 ;; Index todos by todolist
			 (basecamp-map-xml-children
			  (car (xml-get-children xml 'todo-items)) 'todo-item '(id content)
			  (lambda (xml id content)
				(puthash (intern todo-list-id)
						 (cons `(,id . ,content)
							   (gethash (intern todo-list-id) todolist-todos))
						 todolist-todos)))))

		  ;; TODO: do not ask for projects or lists where we have no
		  ;; todos

		  ;; Read project, list and todo.
		  (let* ((project-id (basecamp-reverse-cread "Project: " proj-name-id))
				 (todolist-id (basecamp-reverse-cread "List: " (gethash (intern project-id) project-todolists))))
			(basecamp-reverse-cread "Todo: " (gethash (intern todolist-id) todolist-todos)))))))))



(provide 'basecamp)

;;; basecamp ends here
