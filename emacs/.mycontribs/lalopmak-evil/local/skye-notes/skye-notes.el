(require 'evil)
(require 'org)
(require 'notes-capture-templates)


(defvar worf--keyword nil
  "Current `org-mode' keyword, i.e. one of \"TODO\", \"DONE\" etc.")

(defsubst worf-mod-keyword ()
  "Return current keyword."
  worf--keyword)

(defun worf--at-property-p ()
  "Return t if point is at property."
  (looking-at "^:"))

(defvar worf-sharp "^#\\+"
  "Shortcut for the org's #+ regex.")

(defun worf-down (arg)
  "Move ARG headings down."
  (interactive "p")
  (cond ((worf-mod-keyword)
         (worf-dotimes-protect arg
                               (worf--next-keyword (worf-mod-keyword))))
        ((worf--at-property-p)
         (worf--next-property arg))
        ((looking-at worf-sharp)
         (worf--sharp-down))
        (t
         (ignore-errors
           (let ((pt 0)
                 (i 0))
             (while (and (<= (cl-incf i) arg)
                         (> (point) pt))
               (setq pt (point))
               (outline-next-visible-heading 1))
             (unless (= i (1+ arg))
               (message "End reached after %s headings" i)))))))

(defun worf--goto-candidates ()
  (let ((extra (< (buffer-size) 100000))
        candidates)
    (org-map-entries
     (lambda ()
       (let ((comp (org-heading-components))
             (h (org-get-heading)))
         (push
          (cons (format "%d%s%s" (car comp)
                        (make-string (1+ (* 2 (1- (car comp)))) ?\ )
                        (if (get-text-property 0 'fontified h)
                            h
                          (worf--pretty-heading (nth 4 comp) (car comp))))
                (point))
          candidates)
         (when extra
           (save-restriction
             (narrow-to-region
              (progn (org-back-to-heading t) (point))
              (progn (worf-down 1) (point)))
             (save-excursion
               (goto-char (point-min))
               (while (re-search-forward "^#\\+name \\(.*\\)$" nil t)
                 (push (cons (propertize (match-string 1) 'face 'org-meta-line)
                             (line-beginning-position))
                       candidates))))))))
    (nreverse candidates)))

(defun worf--pretty-heading (str lvl)
  "Prettify heading STR or level LVL."
  (setq str (or str ""))
  (setq str (propertize str 'face (nth (1- lvl) org-level-faces)))
  (let (desc)
    (while (and (string-match org-bracket-link-regexp str)
                (stringp (setq desc (match-string 3 str))))
      (setq str (replace-match
                 (propertize desc 'face 'org-link)
                 nil nil str)))
    str))


(defun notes-goto-action (x)
  (goto-char x)
  (notes-re-open-headings))

(defun notes-re-open-headings ()
  (outline-show-children 1000)
  (org-show-subtree)
  (org-show-siblings)
  (recenter))

(defun notes-link ()
  "Link two headings together"
  (interactive)
  (require 'helm-multi-match)
  (let ((cands (worf--goto-candidates)) 
        (helm-update-blacklist-regexps
         helm-candidate-number-limit))
    (helm :sources
          `((name . "Link Headings")
            (candidates . ,cands)
            (action . notes-link-notes)
            ))))

(defun notes-link-notes (x)
  "Link the note at the current position with the candidate"
  (let ((start (point)))
    (goto-char x)
    (call-interactively 'org-store-link)
    (goto-char start)
    (notes-make-or-goto-links-drawer)
    (org-insert-all-links 1)
    (delete-blank-lines)
    )) 


(defvar notes-link-drawer-name "Links")
(defvar notes-link-drawer-re ":Links:[[:unibyte:]]*?:END:")

(defun notes-make-or-goto-links-drawer ()
  (interactive)
  (org-back-to-heading t)
  (forward-line)
  (when (org-looking-at-p org-property-drawer-re)
    (search-forward-regexp ":END:")
    (next-line))
  (beginning-of-line)
  (if
      (org-looking-at-p ":Links:[[:unibyte:]]*?:END:")
      (progn
        (search-forward-regexp ":END:") 
        (previous-line)
        (end-of-line)
        (insert "\n")
        )
      (org-insert-drawer 'nil notes-link-drawer-name))
  )

(defun worf-goto ()
  "Jump to a heading with `helm'."
  (interactive)
  (require 'helm-multi-match)
  (let* ((cands (worf--goto-candidates)) 
        (helm-update-blacklist-regexps
         helm-candidate-number-limit) 
        (heading-present `((name . "Headings")
                           (candidates . ,cands)
                           (action . notes-goto-action)))
        (heading-not-present `((name . "Headings Fallback")
                               (dummy)
                               (action . notes-insert-heading)))
        )


    (helm :sources
                        )))

(defun notes-new-inbox-header ()
  (interactive)
  (notes-open-buffer)
  (goto-char 0)
  (end-of-line)
  (insert "\n** ")
  (notes-re-open-headings)
  (evil-append 1)
  )

(defun notes-create-or-goto ()
  "Bring up helm and search for a note, then either create a note if none is found or go there")

(defun notes-open-buffer ()
  (interactive)
  (find-file "~/org/zettel.org"))

(provide 'skye-notes)
