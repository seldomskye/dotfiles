(setq lalopmak-evil-packages
      '(
        (skye-notes :location local)
))

(defun lalopmak-evil/init-skye-notes ()
  (use-package skye-notes
    :init
    (progn ())
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
