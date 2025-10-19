;;; redmine.el --- Interactive Redmine issue manager  -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Version: 2.0.0
;; Keywords: tools, redmine
;; Package-Requires: ((emacs "25.1") (elmine "20200520.1237"))

;;; Commentary:

;; Interactive interface for managing Redmine issues and news.
;;
;; Main commands:
;;   M-x redmine/search  - Search issues and news
;;   M-x redmine/issues  - Show your assigned issues
;;   M-x redmine/news    - Show recent news (100 items)
;;
;; Version History:
;; 2.0.0 - 2025-01-19: Major redesign with unified list and detail systems
;; 1.5.0 - 2025-01-19: Add status change for issues in search results
;; 1.4.0 - 2025-01-19: Add assignee selection with completion when creating issues
;; 1.3.0 - 2025-01-19: Add color coding for news dates by recency
;; 1.2.1 - 2025-01-19: Hide internal functions from M-x completion
;; 1.2.0 - 2025-01-19: Add comment functionality to news detail view
;; 1.1.1 - 2025-01-19: Fix date parsing error for date-only formats
;; 1.1.0 - 2025-01-19: Unified search with date sorting, all issue statuses
;; 1.0.0 - Initial version

;;; Code:

(require 'elmine)
(require 'tabulated-list)
(require 'json)

;;; Customization

(defgroup redmine nil
  "Redmine issue manager."
  :group 'tools)

(defcustom redmine-host nil
  "Redmine host URL."
  :type 'string
  :group 'redmine)

(defcustom redmine-api-key nil
  "Redmine API key."
  :type 'string
  :group 'redmine)

(defcustom redmine-todo-status-id 1
  "Status ID for TODO state."
  :type 'integer
  :group 'redmine)

(defcustom redmine-doing-status-id 2
  "Status ID for DOING state."
  :type 'integer
  :group 'redmine)

(defcustom redmine-done-status-id 3
  "Status ID for DONE state."
  :type 'integer
  :group 'redmine)

(defcustom redmine-news-limit 100
  "Maximum number of news items to fetch."
  :type 'integer
  :group 'redmine)

;;; Global variables

(defvar redmine-items nil
  "List of current items (issues and news).")

(defvar redmine-current-item nil
  "Currently selected item.")

(defvar redmine-projects nil
  "Cache of projects.")

(defvar redmine-trackers nil
  "Cache of trackers.")

(defvar redmine-refresh-function nil
  "Function to refresh current list.")

;;; Helper functions for API calls

(defun redmine/api-put-safe (element object path &rest params)
  "Safely perform PUT request, handling empty responses."
  (let* ((params (if (listp (car params)) (car params) params))
         (elmine/host redmine-host)
         (elmine/api-key redmine-api-key)
         (data (elmine/api-encode `(,element ,object)))
         (response (elmine/api-raw "PUT" path data params))
         (status (elmine/get response :status :code)))
    (cond ((or (eq status 200) (eq status 204))
           t)
          ((eq status 404)
           (error "Resource not found"))
          ((eq status 422)
           (error "Unprocessable entity - check your data"))
          (t
           (error "HTTP error %d" status)))))

(defun redmine/update-issue-safe (update-plist)
  "Safely update issue with UPDATE-PLIST."
  (let* ((id (plist-get update-plist :id))
         (path (format "/issues/%s.json" id))
         (object (copy-sequence update-plist)))
    (setq object (plist-put object :id nil))
    (redmine/api-put-safe :issue object path)))

;;; Project and metadata functions

(defun redmine/fetch-projects ()
  "Fetch and cache projects."
  (unless (and redmine-host redmine-api-key)
    (error "Please set redmine-host and redmine-api-key first"))
  (unless redmine-projects
    (let ((elmine/host redmine-host)
          (elmine/api-key redmine-api-key))
      (setq redmine-projects (elmine/get-projects :limit t))))
  redmine-projects)

(defun redmine/fetch-trackers ()
  "Fetch and cache trackers."
  (unless (and redmine-host redmine-api-key)
    (error "Please set redmine-host and redmine-api-key first"))
  (unless redmine-trackers
    (let ((elmine/host redmine-host)
          (elmine/api-key redmine-api-key))
      (setq redmine-trackers (elmine/get-trackers))))
  redmine-trackers)

(defun redmine/complete-project ()
  "Complete project name and return project identifier."
  (let* ((projects (redmine/fetch-projects))
         (project-alist (mapcar (lambda (p)
                                  (cons (format "%s - %s"
                                               (plist-get p :name)
                                               (plist-get p :identifier))
                                        (plist-get p :identifier)))
                                projects))
         (selection (completing-read "Project: " project-alist nil t)))
    (cdr (assoc selection project-alist))))

(defun redmine/complete-tracker ()
  "Complete tracker name and return tracker ID."
  (let* ((trackers (redmine/fetch-trackers))
         (tracker-alist (mapcar (lambda (tr)
                                  (cons (plist-get tr :name)
                                        (plist-get tr :id)))
                                trackers))
         (selection (completing-read "Tracker: " tracker-alist nil t)))
    (cdr (assoc selection tracker-alist))))

(defun redmine/fetch-project-members (project-id)
  "Fetch members of PROJECT-ID."
  (condition-case err
      (let ((elmine/host redmine-host)
            (elmine/api-key redmine-api-key))
        (elmine/api-get-all :memberships 
                           (format "/projects/%s/memberships.json" project-id)))
    (error
     (message "Error fetching members: %s" (error-message-string err))
     nil)))

(defun redmine/complete-assignee (project-id)
  "Complete assignee name for PROJECT-ID and return user ID."
  (let* ((members (redmine/fetch-project-members project-id))
         (user-alist (mapcar (lambda (m)
                              (let ((user (plist-get m :user)))
                                (when user
                                  (cons (plist-get user :name)
                                        (plist-get user :id)))))
                            members))
         (user-alist (delq nil user-alist)))
    (if user-alist
        (let ((selection (completing-read "Assignee (optional): " 
                                         (cons '("(none)" . nil) user-alist)
                                         nil t)))
          (if (string= selection "(none)")
              nil
            (cdr (assoc selection user-alist))))
      nil)))

;;; Date utilities

(defun redmine/days-since (date-string)
  "Calculate days since DATE-STRING from now."
  (when date-string
    (condition-case nil
        (let* ((date (date-to-time date-string))
               (now (current-time))
               (diff (time-subtract now date))
               (days (/ (float-time diff) 86400)))
          (floor days))
      (error nil))))

(defun redmine/days-until (date-string)
  "Calculate days until DATE-STRING from now."
  (when date-string
    (condition-case nil
        (let* ((date (date-to-time date-string))
               (now (current-time))
               (diff (time-subtract date now))
               (days (/ (float-time diff) 86400)))
          (floor days))
      (error nil))))

(defun redmine/format-due-date (date-string)
  "Format due date with color based on proximity."
  (if (not date-string)
      ""
    (condition-case nil
        (let* ((days (redmine/days-until date-string))
               (formatted (format-time-string "%Y-%m-%d" (date-to-time date-string)))
               (face (cond
                      ((not days) '(:foreground "dark slate gray"))
                      ((< days 0) '(:foreground "red" :weight bold))
                      ((<= days 3) '(:foreground "red"))
                      ((<= days 7) '(:foreground "dark orange"))
                      ((<= days 14) '(:foreground "goldenrod"))
                      ((<= days 30) '(:foreground "dark green"))
                      (t '(:foreground "dark slate gray")))))
          (propertize formatted 'face face))
      (error date-string))))

(defun redmine/format-date (date-string)
  "Format DATE-STRING for display."
  (if date-string
      (condition-case nil
          (format-time-string "%Y-%m-%d" (date-to-time date-string))
        (error date-string))
    ""))

(defun redmine/format-news-date (date-string)
  "Format news date with color based on recency."
  (if (not date-string)
      ""
    (condition-case nil
        (let* ((days (redmine/days-since date-string))
               (formatted (format-time-string "%Y-%m-%d" (date-to-time date-string)))
               (face (cond
                      ((not days) nil)
                      ((= days 0) '(:foreground "red" :weight bold))
                      ((<= days 3) '(:foreground "dark orange"))
                      ((<= days 7) '(:foreground "goldenrod"))
                      ((<= days 30) '(:foreground "dark green"))
                      (t '(:foreground "dark slate gray")))))
          (if face
              (propertize formatted 'face face)
            formatted))
      (error date-string))))

;;; Unified list mode (for issues and news)

(defvar redmine-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") (lambda () (interactive) (redmine/open-item)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") (lambda () (interactive) (redmine/refresh-list)))
    (define-key map (kbd "a") (lambda () (interactive) (redmine/new-item)))
    (define-key map (kbd "A") (lambda () (interactive) (redmine/change-assignee)))
    (define-key map (kbd "t") (lambda () (interactive) (redmine/set-status redmine-todo-status-id)))
    (define-key map (kbd "d") (lambda () (interactive) (redmine/set-status redmine-doing-status-id)))
    (define-key map (kbd "D") (lambda () (interactive) (redmine/set-status redmine-done-status-id)))
    (define-key map (kbd "c") (lambda () (interactive) (redmine/add-comment-at-point)))
    map)
  "Keymap for redmine unified list mode.")

(define-derived-mode redmine-list-mode tabulated-list-mode "Redmine"
  "Major mode for displaying Redmine items (issues and news)."
  (setq tabulated-list-format
        [("Date" 12 t)
         ("Type" 6 t)
         ("Project" 15 t)
         ("Status" 10 t)
         ("Title" 40 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Date" nil))
  (tabulated-list-init-header))

(defun redmine/item-to-entry (item)
  "Convert ITEM (issue or news) to tabulated-list entry."
  (let* ((type (plist-get item :type))
         (id (plist-get item :id))
         (date (plist-get item :date))
         (project (plist-get item :project))
         (status (plist-get item :status))
         (title (plist-get item :title)))
    (list (cons type id)
          (vector date
                  (propertize type 'face 
                            (if (string= type "Issue")
                                '(:foreground "cyan" :weight bold)
                              '(:foreground "green" :weight bold)))
                  (or project "")
                  (or status "")
                  (or title "")))))

(defun redmine/issue-to-item (issue)
  "Convert ISSUE to unified item format."
  (let* ((due-date (plist-get issue :due_date))
         (updated (plist-get issue :updated_on))
         (sort-date (or due-date updated ""))
         (display-date (if due-date
                          (redmine/format-due-date due-date)
                        (redmine/format-news-date updated))))
    `(:type "Issue"
      :id ,(plist-get issue :id)
      :date ,display-date
      :date-sort ,sort-date
      :project ,(elmine/get issue :project :name)
      :status ,(elmine/get issue :status :name)
      :title ,(plist-get issue :subject)
      :data ,issue)))

(defun redmine/news-to-item (news)
  "Convert NEWS to unified item format."
  (let ((created (plist-get news :created_on)))
    `(:type "News"
      :id ,(plist-get news :id)
      :date ,(redmine/format-news-date created)
      :date-sort ,(or created "")
      :project ,(elmine/get news :project :name)
      :status ""
      :title ,(plist-get news :title)
      :data ,news)))

(defun redmine/display-items (buffer-name items refresh-fn)
  "Display ITEMS in BUFFER-NAME with REFRESH-FN."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (redmine-list-mode)
      (setq redmine-items items)
      (setq redmine-refresh-function refresh-fn)
      (if items
          (let ((sorted-items (sort (copy-sequence items)
                                   (lambda (a b)
                                     (let ((date-a (plist-get a :date-sort))
                                           (date-b (plist-get b :date-sort)))
                                       (string> date-a date-b))))))
            (setq tabulated-list-entries
                  (mapcar #'redmine/item-to-entry sorted-items))
            (tabulated-list-print t)
            (save-excursion
              (goto-char (point-min))
              (let ((inhibit-read-only t))
                (insert (propertize "Color Legend: " 'face 'bold))
                (insert (propertize "●" 'face '(:foreground "red" :weight bold)) " Today  ")
                (insert (propertize "●" 'face '(:foreground "dark orange")) " 1-3d  ")
                (insert (propertize "●" 'face '(:foreground "goldenrod")) " 4-7d  ")
                (insert (propertize "●" 'face '(:foreground "dark green")) " 8-30d  ")
                (insert (propertize "●" 'face '(:foreground "dark slate gray")) " 31d+\n")
                (insert (propertize "Key Bindings: " 'face 'bold))
                (insert "RET:Open  a:New  A:Assignee  t/d/D:Status  c:Comment  g:Refresh  q:Quit\n")
                (insert (propertize (make-string 90 ?―) 'face 'shadow))
                (insert "\n")))
            (goto-char (point-min))
            (message "Loaded %d items" (length items)))
        (erase-buffer)
        (insert "No items found.\n")
        (message "No items found")))
    (switch-to-buffer buffer)))

;;; List actions

(defun redmine/get-item-at-point ()
  "Get item at point."
  (let* ((composite-id (tabulated-list-get-id))
         (type (car composite-id))
         (id (cdr composite-id)))
    (when (and type id)
      (cl-find-if (lambda (i)
                   (and (string= (plist-get i :type) type)
                        (= (plist-get i :id) id)))
                 redmine-items))))

(defun redmine/open-item ()
  "Open item at point."
  (let ((item (redmine/get-item-at-point)))
    (if (not item)
        (message "No item at point")
      (let ((type (plist-get item :type))
            (id (plist-get item :id)))
        (message "Loading %s #%d..." type id)
        (condition-case err
            (let ((elmine/host redmine-host)
                  (elmine/api-key redmine-api-key))
              (cond
               ((string= type "Issue")
                (let ((full-issue (elmine/get-issue id :include "journals")))
                  (when full-issue
                    (redmine/show-detail full-issue "Issue"))))
               ((string= type "News")
                (let ((full-news (elmine/api-get :news (format "/news/%s.json" id) :include "comments")))
                  (when full-news
                    (redmine/show-detail full-news "News"))))))
          (error
           (message "Error loading %s: %s" type (error-message-string err))))))))

(defun redmine/refresh-list ()
  "Refresh current list."
  (if redmine-refresh-function
      (funcall redmine-refresh-function)
    (message "Cannot refresh this buffer")))

(defun redmine/new-item ()
  "Create new issue or news."
  (let ((type (completing-read "Type: " '("Issue" "News") nil t)))
    (cond
     ((string= type "Issue")
      (redmine/create-issue))
     ((string= type "News")
      (redmine/create-news)))))

(defun redmine/change-assignee ()
  "Change assignee of issue at point."
  (let ((item (redmine/get-item-at-point)))
    (if (not item)
        (message "No item at point")
      (let ((type (plist-get item :type)))
        (if (not (string= type "Issue"))
            (message "Cannot change assignee of news items")
          (let* ((issue (plist-get item :data))
                 (id (plist-get issue :id))
                 (project-id (elmine/get issue :project :id))
                 (new-assignee (redmine/complete-assignee project-id)))
            (condition-case err
                (progn
                  (message "Updating assignee for issue #%d..." id)
                  (if new-assignee
                      (redmine/update-issue-safe `(:id ,id :assigned_to_id ,new-assignee))
                    (redmine/update-issue-safe `(:id ,id :assigned_to_id "")))
                  (message "Assignee updated for issue #%d" id)
                  (sit-for 0.5)
                  (redmine/refresh-list))
              (error
               (message "Error updating assignee: %s" (error-message-string err))))))))))

(defun redmine/set-status (status-id)
  "Set status for issue at point."
  (let ((item (redmine/get-item-at-point)))
    (if (not item)
        (message "No item at point")
      (let ((type (plist-get item :type)))
        (if (not (string= type "Issue"))
            (message "Cannot change status of news items")
          (let* ((issue (plist-get item :data))
                 (id (plist-get issue :id)))
            (condition-case err
                (progn
                  (message "Updating status for issue #%d..." id)
                  (redmine/update-issue-safe `(:id ,id :status_id ,status-id))
                  (message "Status updated for issue #%d" id)
                  (sit-for 0.5)
                  (redmine/refresh-list))
              (error
               (message "Error updating status: %s" (error-message-string err))))))))))

(defun redmine/add-comment-at-point ()
  "Add comment to item at point."
  (let ((item (redmine/get-item-at-point)))
    (if (not item)
        (message "No item at point")
      (let ((type (plist-get item :type))
            (data (plist-get item :data)))
        (cond
         ((string= type "Issue")
          (redmine/add-issue-comment data))
         ((string= type "News")
          (redmine/add-news-comment data)))))))

;;; Detail view

(defvar redmine-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "c") (lambda () (interactive) (redmine/add-comment-in-detail)))
    (define-key map (kbd "e") (lambda () (interactive) (redmine/edit-detail)))
    map)
  "Keymap for redmine detail mode.")

(define-derived-mode redmine-detail-mode special-mode "Redmine-Detail"
  "Major mode for viewing Redmine item details.")

(defun redmine/show-detail (item type)
  "Show detail view for ITEM of TYPE."
  (let ((buffer (get-buffer-create "*Redmine Detail*")))
    (setq redmine-current-item (cons type item))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (redmine-detail-mode)
        (cond
         ((string= type "Issue")
          (redmine/insert-issue-detail item))
         ((string= type "News")
          (redmine/insert-news-detail item)))
        (insert "\n")
        (insert (propertize (make-string 70 ?=) 'face 'bold))
        (insert "\n\n")
        (insert (propertize "Key Bindings:\n" 'face 'bold))
        (insert "  c  - Add comment\n")
        (insert "  e  - Edit\n")
        (insert "  q  - Quit\n")
        (goto-char (point-min))))
    (switch-to-buffer buffer)))

(defun redmine/insert-issue-detail (issue)
  "Insert ISSUE details into current buffer."
  (let ((id (plist-get issue :id))
        (subject (plist-get issue :subject))
        (description (or (plist-get issue :description) ""))
        (status (elmine/get issue :status :name))
        (priority (elmine/get issue :priority :name))
        (assigned (or (elmine/get issue :assigned_to :name) "Unassigned"))
        (project (elmine/get issue :project :name))
        (due-date (plist-get issue :due_date))
        (created (redmine/format-date (plist-get issue :created_on)))
        (updated (redmine/format-date (plist-get issue :updated_on)))
        (journals (plist-get issue :journals)))
    (insert (propertize (format "Issue #%d: %s\n" id subject)
                       'face '(:height 1.3 :weight bold)))
    (insert (propertize (make-string 70 ?=) 'face 'bold))
    (insert "\n\n")
    (insert (propertize "Project: " 'face 'bold) project "\n")
    (insert (propertize "Status: " 'face 'bold) status "\n")
    (insert (propertize "Priority: " 'face 'bold) priority "\n")
    (insert (propertize "Assigned to: " 'face 'bold) assigned "\n")
    (when due-date
      (insert (propertize "Due date: " 'face 'bold) (redmine/format-due-date due-date) "\n"))
    (insert (propertize "Created: " 'face 'bold) created "\n")
    (insert (propertize "Updated: " 'face 'bold) updated "\n\n")
    (insert (propertize (make-string 70 ?-) 'face 'bold))
    (insert "\n\n")
    (insert (propertize "Description:\n" 'face '(:weight bold :underline t)))
    (insert description "\n\n")
    (when journals
      (insert (propertize (make-string 70 ?=) 'face 'bold))
      (insert "\n\n")
      (insert (propertize "Comments:\n" 'face '(:height 1.2 :weight bold)))
      (insert (propertize (make-string 70 ?-) 'face 'bold))
      (insert "\n\n")
      (let ((comments (cl-remove-if-not
                      (lambda (j)
                        (and (plist-get j :notes)
                             (> (length (plist-get j :notes)) 0)))
                      journals)))
        (if comments
            (dolist (comment comments)
              (let* ((author (elmine/get comment :user :name))
                     (date (redmine/format-date (plist-get comment :created_on)))
                     (notes (plist-get comment :notes)))
                (insert (propertize (format "%s - %s\n" author date)
                                   'face '(:foreground "steel blue" :weight bold)))
                (insert notes "\n\n")))
          (insert (propertize "No comments yet.\n" 'face 'italic)))))))

(defun redmine/insert-news-detail (news)
  "Insert NEWS details into current buffer."
  (let ((id (plist-get news :id))
        (title (plist-get news :title))
        (summary (plist-get news :summary))
        (description (plist-get news :description))
        (project (elmine/get news :project :name))
        (author (elmine/get news :author :name))
        (created (redmine/format-date (plist-get news :created_on)))
        (comments (plist-get news :comments)))
    (insert (propertize (format "News #%d: %s\n" id title)
                       'face '(:height 1.3 :weight bold)))
    (insert (propertize (make-string 70 ?=) 'face 'bold))
    (insert "\n\n")
    (insert (propertize "Project: " 'face 'bold) project "\n")
    (insert (propertize "Author: " 'face 'bold) author "\n")
    (insert (propertize "Date: " 'face 'bold) created "\n\n")
    (insert (propertize (make-string 70 ?-) 'face 'bold))
    (insert "\n\n")
    (when summary
      (insert (propertize "Summary:\n" 'face 'bold))
      (insert summary "\n\n"))
    (insert (propertize "Description:\n" 'face '(:weight bold :underline t)))
    (insert description "\n\n")
    (when comments
      (insert (propertize (make-string 70 ?=) 'face 'bold))
      (insert "\n\n")
      (insert (propertize "Comments:\n" 'face '(:height 1.2 :weight bold)))
      (insert (propertize (make-string 70 ?-) 'face 'bold))
      (insert "\n\n")
      (dolist (comment comments)
        (let* ((comment-author (elmine/get comment :author :name))
               (comment-date (redmine/format-date (plist-get comment :created_on)))
               (comment-text (plist-get comment :comments)))
          (insert (propertize (format "%s - %s\n" comment-author comment-date)
                             'face '(:foreground "steel blue" :weight bold)))
          (insert comment-text "\n\n"))))))

(defun redmine/add-comment-in-detail ()
  "Add comment to current item in detail view."
  (if (not redmine-current-item)
      (message "No current item")
    (let ((type (car redmine-current-item))
          (data (cdr redmine-current-item)))
      (cond
       ((string= type "Issue")
        (redmine/add-issue-comment data))
       ((string= type "News")
        (redmine/add-news-comment data))))))

(defun redmine/edit-detail ()
  "Edit current item in detail view."
  (message "Edit functionality not yet implemented"))

;;; Comment functions

(defun redmine/read-multiline-text (prompt callback)
  "Read multiline text in a dedicated buffer with PROMPT and CALLBACK."
  (let ((buffer (get-buffer-create "*Redmine Text Input*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (text-mode)
      (setq header-line-format
            (propertize "C-c C-c: Finish  C-c C-k: Cancel"
                       'face '(:background "dark slate gray" :foreground "white")))
      (let ((start (point)))
        (insert prompt "\n")
        (insert (make-string 70 ?-) "\n\n")
        (add-text-properties start (point) '(read-only t front-sticky t rear-nonsticky t face bold)))
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (let* ((header-end (save-excursion
                                           (goto-char (point-min))
                                           (forward-line 2)
                                           (point)))
                              (text (buffer-substring-no-properties header-end (point-max))))
                         (kill-buffer)
                         (when callback
                           (funcall callback text)))))
      (local-set-key (kbd "C-c C-k")
                     (lambda ()
                       (interactive)
                       (when (yes-or-no-p "Cancel input? ")
                         (kill-buffer)
                         (message "Cancelled")))))
    (switch-to-buffer buffer)
    (goto-char (point-max))))

(defun redmine/add-issue-comment (issue)
  "Add comment to ISSUE."
  (let ((id (plist-get issue :id)))
    (redmine/read-multiline-text
     (format "Comment for issue #%d (C-c C-c to finish, C-c C-k to cancel):" id)
     (lambda (comment)
       (when (and comment (> (length (string-trim comment)) 0))
         (condition-case err
             (progn
               (message "Adding comment to issue #%d..." id)
               (redmine/update-issue-safe `(:id ,id :notes ,comment))
               (message "Comment added to issue #%d" id)
               (sit-for 0.5)
               (when (get-buffer "*Redmine Detail*")
                 (let* ((elmine/host redmine-host)
                        (elmine/api-key redmine-api-key)
                        (full-issue (elmine/get-issue id :include "journals")))
                   (when full-issue
                     (redmine/show-detail full-issue "Issue")))))
           (error
            (message "Error adding comment: %s" (error-message-string err)))))))))

(defun redmine/add-news-comment (news)
  "Add comment to NEWS."
  (let ((id (plist-get news :id)))
    (redmine/read-multiline-text
     (format "Comment for news #%d (C-c C-c to finish, C-c C-k to cancel):" id)
     (lambda (comment)
       (when (and comment (> (length (string-trim comment)) 0))
         (condition-case err
             (let* ((elmine/host redmine-host)
                    (elmine/api-key redmine-api-key)
                    (comment-data `(:comments ,comment))
                    (path (format "/news/%s/comments.json" id))
                    (data (elmine/api-encode `(:comment ,comment-data)))
                    (response (elmine/api-raw "POST" path data nil))
                    (status (elmine/get response :status :code)))
               (if (or (eq status 200) (eq status 201) (eq status 204))
                   (progn
                     (message "Comment added to news #%d" id)
                     (sit-for 0.5)
                     (when (get-buffer "*Redmine Detail*")
                       (let ((full-news (elmine/api-get :news (format "/news/%s.json" id) :include "comments")))
                         (when full-news
                           (redmine/show-detail full-news "News")))))
                 (message "Failed to add comment: HTTP status %d" status)))
           (error
            (message "Error adding comment: %s" (error-message-string err)))))))))

;;; Creation functions

(defun redmine/create-issue ()
  "Create a new issue."
  (condition-case err
      (let* ((project (redmine/complete-project))
             (tracker (redmine/complete-tracker))
             (assignee (redmine/complete-assignee project))
             (subject (read-string "Subject: "))
             (due-date (read-string "Due Date (YYYY-MM-DD, optional): "))
             (description (read-string "Description: "))
             (issue-data `(:project_id ,project
                          :tracker_id ,tracker
                          :subject ,subject
                          :description ,description)))
        (when (string= subject "")
          (error "Subject cannot be empty"))
        (when assignee
          (setq issue-data (plist-put issue-data :assigned_to_id assignee)))
        (when (and due-date (> (length due-date) 0))
          (setq issue-data (plist-put issue-data :due_date due-date)))
        (let ((elmine/host redmine-host)
              (elmine/api-key redmine-api-key))
          (elmine/create-issue issue-data))
        (message "Issue created successfully")
        (sit-for 0.5)
        (when redmine-refresh-function
          (funcall redmine-refresh-function)))
    (error
     (message "Error creating issue: %s" (error-message-string err)))))

(defun redmine/create-news ()
  "Create a new news item."
  (condition-case err
      (let* ((project (redmine/complete-project))
             (title (read-string "Title: "))
             (summary (read-string "Summary (optional): "))
             (description (read-string "Description: "))
             (news-data `(:title ,title :description ,description))
             (path (format "/projects/%s/news.json" project)))
        (when (string= title "")
          (error "Title cannot be empty"))
        (when (and summary (> (length summary) 0))
          (setq news-data (plist-put news-data :summary summary)))
        (let* ((elmine/host redmine-host)
               (elmine/api-key redmine-api-key)
               (data (elmine/api-encode `(:news ,news-data)))
               (response (elmine/api-raw "POST" path data nil))
               (status (elmine/get response :status :code)))
          (if (or (eq status 200) (eq status 201) (eq status 204))
              (progn
                (message "News created successfully")
                (sit-for 0.5)
                (when redmine-refresh-function
                  (funcall redmine-refresh-function)))
            (error "Failed to create news: HTTP status %d" status))))
    (error
     (message "Error creating news: %s" (error-message-string err)))))

;;; Main entry points

;;;###autoload
(defun redmine/search ()
  "Search issues and news."
  (interactive)
  (unless (and redmine-host redmine-api-key)
    (error "Please set redmine-host and redmine-api-key"))
  (let* ((query (read-string "Search (issues and news): "))
         (elmine/host redmine-host)
         (elmine/api-key redmine-api-key)
         (items '()))
    (when (and query (> (length query) 0))
      (message "Searching...")
      ;; Search issues
      (condition-case err
          (let ((issue-results (elmine/get-issues 
                               :subject (concat "~" query) 
                               :status_id "*"
                               :limit t)))
            (when issue-results
              (setq items (append items (mapcar #'redmine/issue-to-item issue-results)))))
        (error
         (message "Error searching issues: %s" (error-message-string err))))
      ;; Search news
      (condition-case err
          (let* ((all-news (elmine/api-get-all :news "/news.json" :limit t))
                 (news-results (when all-news
                                (cl-remove-if-not
                                 (lambda (news)
                                   (or (and (plist-get news :title)
                                           (string-match-p query (plist-get news :title)))
                                       (and (plist-get news :description)
                                           (string-match-p query (plist-get news :description)))))
                                 all-news))))
            (when news-results
              (setq items (append items (mapcar #'redmine/news-to-item news-results)))))
        (error
         (message "Error searching news: %s" (error-message-string err))))
      (redmine/display-items "*Redmine Search*" items
                            (lambda () (redmine/search))))))

;;;###autoload
(defun redmine/issues ()
  "Show your assigned issues."
  (interactive)
  (unless (and redmine-host redmine-api-key)
    (error "Please set redmine-host and redmine-api-key"))
  (message "Fetching your issues...")
  (condition-case err
      (let* ((elmine/host redmine-host)
             (elmine/api-key redmine-api-key)
             (issues (elmine/get-issues :assigned_to_id "me" :status_id "*" :limit t))
             (items (mapcar #'redmine/issue-to-item issues)))
        (redmine/display-items "*Redmine Issues*" items
                              (lambda () (redmine/issues))))
    (error
     (message "Error fetching issues: %s" (error-message-string err)))))

;;;###autoload
(defun redmine/news ()
  "Show recent news (100 items)."
  (interactive)
  (unless (and redmine-host redmine-api-key)
    (error "Please set redmine-host and redmine-api-key"))
  (message "Fetching recent news...")
  (condition-case err
      (let* ((elmine/host redmine-host)
             (elmine/api-key redmine-api-key)
             (news (elmine/api-get-all :news "/news.json" :limit redmine-news-limit))
             (items (mapcar #'redmine/news-to-item news)))
        (redmine/display-items "*Redmine News*" items
                              (lambda () (redmine/news))))
    (error
     (message "Error fetching news: %s" (error-message-string err)))))

(provide 'redmine)

;;; redmine.el ends here
