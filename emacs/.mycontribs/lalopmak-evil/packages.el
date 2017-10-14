(setq lalopmak-evil-packages
      '(
        (skye-notes :location local)
        (sync-window :location local)
))
(defun lalopmak-evil/set-agenda-commands () 
  (setq org-agenda-custom-commands '(
                                       ("a" "All agenda items" agenda "Agenda"
                                         ((org-agenda-start-on-weekday nil))
                                         ("~/org/agenda.txt"))
                                        ("w" "Work agenda"
                                         ((agenda ""
                                                  ((org-agenda-category-filter-preset
                                                    (quote
                                                     ("+work"))))))
                                         nil)
                                        ("p" "Personal"
                                         ((alltodo ""
                                                   ((org-agenda-category-filter-preset
                                                     (quote
                                                      ("+zettel"))))))
                                         nil nil)
                                        )
                                       ))
(lalopmak-evil/set-agenda-commands)
(defun lalopmak-evil/init-skye-notes ()
  (use-package skye-notes
    :init
    (progn (lalopmak-evil/set-agenda-commands))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'org-mode 
        "ob" 'helm-org-in-buffer-headings
        "oa" 'helm-org-agenda-files-headings
        "ol" 'notes-link)
      (spacemacs/set-leader-keys
        "ob" 'notes-open-buffer
        "oo" 'notes-new-inbox-header
        )
      )
    ))

(defun lalopmak-evil/init-sync-window ()
  (use-package sync-window
    :config
    (progn
      (spacemacs/set-leader-keys
        "oz" 'window-sync-init)
        "ox" 'sync-window-mode)
    )
  )
