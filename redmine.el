;;; redmine.el --- Interactive Redmine issue manager

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: tools, redmine
;; Package-Requires: ((emacs "25.1") (elmine "20200520.1237"))

;;; Commentary:

;; Interactive interface for managing Redmine issues.

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

(defcustom redmine-news-limit 50
  "Maximum number of news items to fetch."
  :type 'integer
  :group 'redmine)

;;; Global variables

(defvar redmine-issues nil
  "List of current issues.")

(defvar redmine-current-issue nil
  "Currently selected issue.")

(defvar redmine-projects nil
  "Cache of projects.")

(defvar redmine-trackers nil
  "Cache of trackers.")

(defvar redmine-news nil
  "List of news items.")

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
           (message "Update successful (status %d)" status)
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

;;; Date utilities

(defun redmine/days-until (date-string)
  "Calculate days until DATE-STRING from now."
  (when date-string
    (let* ((date (date-to-time date-string))
           (now (current-time))
           (diff (time-subtract date now))
           (days (/ (float-time diff) 86400)))
      (floor days))))

(defun redmine/format-due-date (date-string)
  "Format due date with color based on proximity."
  (if (not date-string)
      ""
    (let* ((days (redmine/days-until date-string))
           (formatted (format-time-string "%Y-%m-%d" (date-to-time date-string)))
           (face (cond
                  ((< days 0) '(:foreground "red" :weight bold))
                  ((<= days 3) '(:foreground "red"))
                  ((<= days 7) '(:foreground "dark orange"))
                  ((<= days 14) '(:foreground "goldenrod"))
                  ((<= days 30) '(:foreground "dark green"))
                  (t '(:foreground "dark slate gray")))))
      (propertize formatted 'face face))))

;;; Issue list mode

(defvar redmine-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") (lambda () (interactive) (redmine/open-issue)))
    (define-key map (kbd "g") (lambda () (interactive) (redmine/refresh)))
    (define-key map (kbd "n") (lambda () (interactive) (redmine/new-issue)))
    (define-key map (kbd "s") (lambda () (interactive) (redmine/search-issues)))
    (define-key map (kbd "t") (lambda () (interactive) (redmine/set-status redmine-todo-status-id)))
    (define-key map (kbd "d") (lambda () (interactive) (redmine/set-status redmine-doing-status-id)))
    (define-key map (kbd "D") (lambda () (interactive) (redmine/set-status redmine-done-status-id)))
    (define-key map (kbd "c") (lambda () (interactive) (redmine/add-comment)))
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for redmine list mode.")

(define-derived-mode redmine-list-mode tabulated-list-mode "Redmine"
  "Major mode for listing Redmine issues (internal use only)."
  (setq tabulated-list-format
        [("#" 6 t)
         ("Status" 12 t)
         ("Priority" 10 t)
         ("Due Date" 12 t)
         ("Subject" 40 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Due Date" nil))
  (tabulated-list-init-header))

;; Hide from M-x completion
(put 'redmine-list-mode 'completion-predicate (lambda (_ _) nil))

(defun redmine/fetch-issues ()
  "Fetch open issues assigned to current user."
  (condition-case err
      (let ((elmine/host redmine-host)
            (elmine/api-key redmine-api-key))
        (elmine/get-issues :assigned_to_id "me" :status_id "open" :limit t))
    (error
     (message "Error fetching issues: %s" (error-message-string err))
     nil)))

(defun redmine/format-date (date-string)
  "Format DATE-STRING for display."
  (if date-string
      (condition-case nil
          (format-time-string "%Y-%m-%d" (date-to-time date-string))
        (error date-string))
    ""))

(defun redmine/issue-to-entry (issue)
  "Convert ISSUE to tabulated-list entry."
  (let* ((id (plist-get issue :id))
         (subject (plist-get issue :subject))
         (status (elmine/get issue :status :name))
         (priority (elmine/get issue :priority :name))
         (due-date (plist-get issue :due_date))
         (due-display (redmine/format-due-date due-date))
         ;; For sorting, use days as number (9999 if no due date)
         (due-sort (if due-date
                      (or (redmine/days-until due-date) 9999)
                    9999)))
    (list id
          (vector (format "%d" id)
                  (or status "")
                  (or priority "")
                  (list due-display 'sort-key due-sort)
                  (or subject "")))))

(defun redmine/refresh-list ()
  "Refresh issue list."
  (message "Fetching issues...")
  (setq redmine-issues (redmine/fetch-issues))
  (if redmine-issues
      (progn
        (setq tabulated-list-entries
              (mapcar #'redmine/issue-to-entry redmine-issues))
        (tabulated-list-print t)
        (save-excursion
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (insert (propertize "Key Bindings: " 'face 'bold))
            (insert "RET:Open  n:New  s:Search  g:Refresh  t/d/D:Status  c:Comment  q:Quit\n")
            (insert (propertize (make-string 80 ?―) 'face 'shadow))
            (insert "\n")))
        (goto-char (point-min))
        (message "Loaded %d open issues" (length redmine-issues)))
    (message "No open issues found or error occurred")))

;;;###autoload
(defun redmine/show-my-issues ()
  "Show open issues assigned to me."
  (interactive)
  (unless (and redmine-host redmine-api-key)
    (error "Please set redmine-host and redmine-api-key"))
  (let ((buffer (get-buffer-create "*Redmine Issues*")))
    (with-current-buffer buffer
      (redmine-list-mode)
      (redmine/refresh-list)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun redmine/refresh ()
  "Refresh issue list."
  (redmine/refresh-list))

(defun redmine/get-issue-at-point ()
  "Get issue at point."
  (let ((id (tabulated-list-get-id)))
    (when id
      (cl-find-if (lambda (issue)
                    (= (plist-get issue :id) id))
                  redmine-issues))))

;;; New issue creation

(defvar redmine-multiline-text nil
  "Temporary storage for multiline text input.")

(defvar redmine-multiline-callback nil
  "Callback function for multiline text input.")

(defun redmine/read-multiline-text (prompt callback)
  "Read multiline text in a dedicated buffer with PROMPT and CALLBACK."
  (setq redmine-multiline-callback callback)
  (let ((buffer (get-buffer-create "*Redmine Text Input*")))
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode)
      (insert (propertize prompt 'face 'bold 'read-only t))
      (insert "\n")
      (insert (propertize (make-string 70 ?-) 'face 'bold 'read-only t))
      (insert "\n\n")
      (setq header-line-format
            (propertize "C-c C-c: Finish  C-c C-k: Cancel"
                       'face '(:background "dark slate gray" :foreground "white")))
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (let ((text (buffer-substring-no-properties
                                   (save-excursion
                                     (goto-char (point-min))
                                     (forward-line 3)
                                     (point))
                                   (point-max))))
                         (kill-buffer)
                         (when redmine-multiline-callback
                           (funcall redmine-multiline-callback text)))))
      (local-set-key (kbd "C-c C-k")
                     (lambda ()
                       (interactive)
                       (when (yes-or-no-p "Cancel input? ")
                         (kill-buffer)
                         (message "Cancelled")))))
    (switch-to-buffer buffer)
    (goto-char (point-max))))
  (let* ((project (redmine/complete-project))
         (tracker (redmine/complete-tracker))
         (subject (read-string "Subject: "))
         (description (read-string "Description: "))
         (due-date (read-string "Due date (YYYY-MM-DD, optional): ")))
    (when (and subject (> (length subject) 0))
      (let ((elmine/host redmine-host)
            (elmine/api-key redmine-api-key)
            (issue-data `(:project_id ,project
                         :tracker_id ,tracker
                         :subject ,subject
                         :description ,description)))
        (when (and due-date (> (length due-date) 0))
          (setq issue-data (plist-put issue-data :due_date due-date)))
        (condition-case err
            (progn
              (elmine/create-issue issue-data)
              (message "Issue created successfully")
              (sit-for 0.5)
              (when (get-buffer "*Redmine Issues*")
                (with-current-buffer "*Redmine Issues*"
                  (redmine/refresh-list))))
          (error
           (message "Error creating issue: %s" (error-message-string err))))))))

;;; Search functionality

(defun redmine/search-issues ()
  "Search issues."
  (let* ((query (read-string "Search issues: "))
         (elmine/host redmine-host)
         (elmine/api-key redmine-api-key))
    (when (and query (> (length query) 0))
      (condition-case err
          (let ((results (elmine/get-issues :subject (concat "~" query) :limit t)))
            (if results
                (progn
                  (setq redmine-issues results)
                  (with-current-buffer "*Redmine Issues*"
                    (setq tabulated-list-entries
                          (mapcar #'redmine/issue-to-entry redmine-issues))
                    (tabulated-list-print t)
                    (message "Found %d issues" (length results))))
              (message "No issues found")))
        (error
         (message "Error searching: %s" (error-message-string err)))))))

;;; Status change functions

(defun redmine/set-status (status-id)
  "Set issue status to STATUS-ID."
  (let* ((issue (redmine/get-issue-at-point)))
    (unless issue
      (error "No issue at point"))
    (let ((id (plist-get issue :id)))
      (condition-case err
          (progn
            (message "Updating status for issue #%d..." id)
            (redmine/update-issue-safe `(:id ,id :status_id ,status-id))
            (message "Status updated for issue #%d" id)
            (sit-for 0.5)
            (redmine/refresh-list))
        (error
         (message "Error updating status: %s" (error-message-string err)))))))

;;; Issue detail mode

(defvar redmine-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") (lambda () (interactive) (redmine/save-issue-detail)))
    (define-key map (kbd "C-c C-k") (lambda () (interactive) (redmine/cancel-edit)))
    (define-key map (kbd "C-c C-a") (lambda () (interactive) (redmine/add-comment-from-detail)))
    (define-key map (kbd "q") (lambda () (interactive) (redmine/cancel-edit)))
    map)
  "Keymap for redmine detail mode.")

(define-derived-mode redmine-detail-mode text-mode "Redmine-Detail"
  "Major mode for editing Redmine issue details (internal use only).")

;; Hide from M-x completion
(put 'redmine-detail-mode 'completion-predicate (lambda (_ _) nil))

(defun redmine/open-issue ()
  "Open issue detail."
  (let ((issue (redmine/get-issue-at-point)))
    (if (not issue)
        (message "No issue at point")
      (let ((id (plist-get issue :id)))
        (message "Loading issue #%d..." id)
        (condition-case err
            (let* ((elmine/host redmine-host)
                   (elmine/api-key redmine-api-key)
                   ;; Include journals (comments) in the response
                   (full-issue (elmine/get-issue id :include "journals")))
              (when full-issue
                (setq redmine-current-issue full-issue)
                (redmine/show-issue-detail full-issue)))
          (error
           (message "Error loading issue: %s" (error-message-string err))))))))

(defun redmine/show-issue-detail (issue)
  "Show ISSUE detail in edit buffer."
  (let ((buffer (get-buffer-create "*Redmine Issue Detail*"))
        (id (plist-get issue :id))
        (subject (plist-get issue :subject))
        (description (or (plist-get issue :description) ""))
        (status-name (elmine/get issue :status :name))
        (priority-name (elmine/get issue :priority :name))
        (assigned-name (or (elmine/get issue :assigned_to :name) "Unassigned"))
        (due-date (plist-get issue :due_date))
        (created (redmine/format-date (plist-get issue :created_on)))
        (updated (redmine/format-date (plist-get issue :updated_on)))
        (journals (plist-get issue :journals)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (redmine-detail-mode)
        (insert (propertize (format "Issue #%d\n" id)
                           'face '(:height 1.5 :weight bold)))
        (insert (propertize (make-string 70 ?=) 'face 'bold))
        (insert "\n\n")
        (insert (propertize "Status: " 'face 'bold))
        (insert status-name "\n")
        (insert (propertize "Priority: " 'face 'bold))
        (insert priority-name "\n")
        (insert (propertize "Assigned to: " 'face 'bold))
        (insert assigned-name "\n")
        (when due-date
          (insert (propertize "Due date: " 'face 'bold))
          (insert (redmine/format-due-date due-date) "\n"))
        (insert (propertize "Created: " 'face 'bold))
        (insert created "\n")
        (insert (propertize "Updated: " 'face 'bold))
        (insert updated "\n\n")
        (insert (propertize (make-string 70 ?-) 'face 'bold))
        (insert "\n\n")
        (insert "---SUBJECT-BEGIN---\n")
        (insert (propertize "Subject:\n" 'face '(:weight bold :underline t)))
        (insert subject "\n")
        (insert "---SUBJECT-END---\n\n")
        (insert "---DESCRIPTION-BEGIN---\n")
        (insert (propertize "Description:\n" 'face '(:weight bold :underline t)))
        (insert description "\n")
        (insert "---DESCRIPTION-END---")
        (insert "\n\n")
        (insert (propertize (make-string 70 ?=) 'face 'bold))
        (insert "\n\n")
        
        ;; Comments section
        (when journals
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
              (insert (propertize "No comments yet.\n\n" 'face 'italic))))
          (insert (propertize (make-string 70 ?-) 'face 'bold))
          (insert "\n\n"))
        
        (insert (propertize "Commands:\n" 'face 'bold))
        (insert "  C-c C-c  - Save changes (subject & description)\n")
        (insert "  C-c C-a  - Add comment\n")
        (insert "  C-c C-k  - Cancel (discard changes)\n")
        (insert "  q        - Quit\n\n")
        (insert (propertize "Note:\n" 'face 'bold))
        (insert "  Use 't', 'd', 'D' in issue list to change status\n")
        (insert "  (switch back with q)\n")
        (goto-char (point-min))
        (search-forward "Subject:" nil t)
        (forward-line 1)))
    (switch-to-buffer buffer)))

(defun redmine/get-field-value (field-name)
  "Get value of FIELD-NAME from current buffer using visible markers."
  (save-excursion
    (goto-char (point-min))
    (let* ((field-upper (upcase (symbol-name field-name)))
           (start-marker (format "---%s-BEGIN---" field-upper))
           (end-marker (format "---%s-END---" field-upper))
           start end)
      (when (search-forward start-marker nil t)
        (forward-line 1)
        (when (looking-at ".*:\n")
          (forward-line 1))
        (setq start (point))
        (when (search-forward end-marker nil t)
          (beginning-of-line)
          (setq end (point))
          (let ((value (buffer-substring-no-properties start end)))
            (string-trim value)))))))

(defun redmine/save-issue-detail ()
  "Save issue changes from detail buffer."
  (condition-case err
      (let* ((issue redmine-current-issue)
             (id (plist-get issue :id))
             (subject (redmine/get-field-value 'subject))
             (description (redmine/get-field-value 'description)))
        (unless (and subject (> (length subject) 0))
          (error "Subject cannot be empty"))
        (message "Saving issue #%d..." id)
        (redmine/update-issue-safe `(:id ,id
                                    :subject ,subject
                                    :description ,(or description "")))
        (message "Issue #%d saved successfully" id)
        (sit-for 1)
        (kill-buffer)
        (when (get-buffer "*Redmine Issues*")
          (with-current-buffer "*Redmine Issues*"
            (redmine/refresh-list))))
    (error
     (message "Error saving issue: %s" (error-message-string err)))))

(defun redmine/cancel-edit ()
  "Cancel editing."
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Discard changes? "))
    (kill-buffer)))

(defun redmine/add-comment-from-detail ()
  "Add comment to current issue from detail buffer."
  (if (not redmine-current-issue)
      (message "No current issue")
    (redmine/add-comment-to-issue redmine-current-issue)))

(defun redmine/add-comment ()
  "Add comment to issue at point."
  (let ((issue (redmine/get-issue-at-point)))
    (if (not issue)
        (message "No issue at point")
      (redmine/add-comment-to-issue issue))))

(defun redmine/add-comment-to-issue (issue)
  "Add comment to ISSUE (internal helper)."
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
               ;; Reload issue detail if it's open
               (when (and (get-buffer "*Redmine Issue Detail*")
                          redmine-current-issue
                          (= (plist-get redmine-current-issue :id) id))
                 (let* ((elmine/host redmine-host)
                        (elmine/api-key redmine-api-key)
                        (full-issue (elmine/get-issue id :include "journals")))
                   (when full-issue
                     (setq redmine-current-issue full-issue)
                     (redmine/show-issue-detail full-issue)))))
           (error
            (message "Error adding comment: %s" (error-message-string err)))))))))

;;; News functionality

(defvar redmine-news-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") (lambda () (interactive) (redmine/open-news)))
    (define-key map (kbd "g") (lambda () (interactive) (redmine/refresh-news)))
    (define-key map (kbd "n") (lambda () (interactive) (redmine/new-news)))
    (define-key map (kbd "s") (lambda () (interactive) (redmine/search-news)))
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for redmine news mode.")

(define-derived-mode redmine-news-mode tabulated-list-mode "Redmine-News"
  "Major mode for listing Redmine news (internal use only)."
  (setq tabulated-list-format
        [("Date" 12 t)
         ("Project" 20 t)
         ("Title" 50 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;; Hide from M-x completion
(put 'redmine-news-mode 'completion-predicate (lambda (_ _) nil))

(defun redmine/fetch-news ()
  "Fetch recent news items (limited by redmine-news-limit)."
  (condition-case err
      (let ((elmine/host redmine-host)
            (elmine/api-key redmine-api-key))
        (elmine/api-get-all :news "/news.json" :limit redmine-news-limit))
    (error
     (message "Error fetching news: %s" (error-message-string err))
     nil)))

(defun redmine/news-to-entry (news-item)
  "Convert NEWS-ITEM to tabulated-list entry."
  (let* ((id (plist-get news-item :id))
         (title (plist-get news-item :title))
         (project-name (elmine/get news-item :project :name))
         (created (redmine/format-date (plist-get news-item :created_on))))
    (list id
          (vector created
                  (or project-name "")
                  (or title "")))))

(defun redmine/refresh-news ()
  "Refresh news list."
  (message "Fetching news...")
  (setq redmine-news (redmine/fetch-news))
  (if redmine-news
      (progn
        (setq tabulated-list-entries
              (mapcar #'redmine/news-to-entry redmine-news))
        (tabulated-list-print t)
        (save-excursion
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (insert (propertize "Key Bindings: " 'face 'bold))
            (insert "RET:Open  n:New  s:Search  g:Refresh  q:Quit\n")
            (insert (propertize (make-string 80 ?―) 'face 'shadow))
            (insert "\n")))
        (goto-char (point-min))
        (message "Loaded %d news items (limit: %d)" (length redmine-news) redmine-news-limit))
    (message "No news found or error occurred")))

;;;###autoload
(defun redmine/show-news ()
  "Show all news items."
  (interactive)
  (unless (and redmine-host redmine-api-key)
    (error "Please set redmine-host and redmine-api-key"))
  (let ((buffer (get-buffer-create "*Redmine News*")))
    (with-current-buffer buffer
      (redmine-news-mode)
      (redmine/refresh-news)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun redmine/new-news ()
  "Create a new news item."
  (let* ((project (redmine/complete-project))
         (title (read-string "Title: "))
         (summary (read-string "Summary: ")))
    (when (and title (> (length title) 0))
      (redmine/read-multiline-text
       "Description (C-c C-c to finish, C-c C-k to cancel):"
       (lambda (description)
         (let ((elmine/host redmine-host)
               (elmine/api-key redmine-api-key)
               (news-data `(:title ,title
                           :summary ,summary
                           :description ,description))
               (path (format "/projects/%s/news.json" project)))
           (condition-case err
               (progn
                 ;; Use raw API call to handle empty response
                 (let* ((data (elmine/api-encode `(:news ,news-data)))
                        (response (elmine/api-raw "POST" path data nil))
                        (status (elmine/get response :status :code)))
                   (if (or (eq status 200) (eq status 201) (eq status 204))
                       (progn
                         (message "News created successfully")
                         (sit-for 0.5)
                         (when (get-buffer "*Redmine News*")
                           (with-current-buffer "*Redmine News*"
                             (redmine/refresh-news))))
                     (error "Failed to create news: HTTP status %d" status))))
             (error
              (message "Error creating news: %s" (error-message-string err)))))))))))

(defun redmine/search-news ()
  "Search news items."
  (let ((query (read-string "Search news: ")))
    (when (and query (> (length query) 0))
      (let* ((all-news redmine-news)
             (results (cl-remove-if-not
                      (lambda (news)
                        (string-match-p query (plist-get news :title)))
                      all-news)))
        (if results
            (progn
              (setq tabulated-list-entries
                    (mapcar #'redmine/news-to-entry results))
              (tabulated-list-print t)
              (message "Found %d news items" (length results)))
          (message "No news found"))))))

(defun redmine/open-news ()
  "Open news detail."
  (let* ((id (tabulated-list-get-id))
         (news-item (cl-find-if (lambda (n)
                                  (= (plist-get n :id) id))
                                redmine-news)))
    (when news-item
      (let ((buffer (get-buffer-create "*Redmine News Detail*"))
            (title (plist-get news-item :title))
            (summary (plist-get news-item :summary))
            (description (plist-get news-item :description))
            (project (elmine/get news-item :project :name))
            (author (elmine/get news-item :author :name))
            (created (redmine/format-date (plist-get news-item :created_on))))
        (with-current-buffer buffer
          (erase-buffer)
          (special-mode)
          (let ((inhibit-read-only t))
            (insert (propertize title 'face '(:height 1.5 :weight bold)))
            (insert "\n\n")
            (insert (propertize "Project: " 'face 'bold))
            (insert project "\n")
            (insert (propertize "Author: " 'face 'bold))
            (insert author "\n")
            (insert (propertize "Date: " 'face 'bold))
            (insert created "\n\n")
            (insert (propertize (make-string 70 ?-) 'face 'bold))
            (insert "\n\n")
            (when summary
              (insert (propertize "Summary:\n" 'face 'bold))
              (insert summary "\n\n"))
            (when description
              (insert (propertize "Description:\n" 'face 'bold))
              (insert description "\n"))
            (goto-char (point-min))))
        (switch-to-buffer buffer)))))

(provide 'redmine)

;;; redmine.el ends here
