(setq lalopmak-evil-packages
      '(
        (lalopmak-evil :location local)
        (lalopmak-evil-org-mode :location local)
        (skye-notes :location local)
        (evil-org-mode :excluded t)
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

(defun lalopmak-evil/init-lalopmak-evil ()
  (use-package lalopmak-evil 
    :init
    (progn
      (require 'helm-config)
      (require 'helm-org)
      (helm-mode 1))
    :config
    (progn
      (global-unset-key (kbd "C-x c"))
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
      (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
      (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

      (when (executable-find "curl")
        (setq helm-google-suggest-use-curl-p t))
      (setq helm-quick-update                     t ; do not display invisible candidates
            helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
            helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
            helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
            helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
            helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
            helm-ff-file-name-history-use-recentf t)

      ;; Replace M-x with the superior version
      (global-set-key (kbd "M-x") 'helm-M-x)
      (global-set-key (kbd "M-y") 'helm-show-kill-ring)
      ;; Change buffer switch to helm
      (set-in-all-evil-states-but-insert "b" 'helm-mini)
      (set-in-all-evil-states-but-insert "B" 'helm-find-files)
      ;; helm-semantic-or-imenu is bound to prefix i, and should probably be moved somewhere better

      ;; helm-register
      (set-in-all-evil-states-but-insert (kbd "M-r") 'helm-register))
    (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags-command))
    (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags))
    (add-to-list 'helm-completing-read-handlers-alist '(org-match-sparse-tree)))

        ) 

(defun lalopmak-evil/init-lalopmak-evil-org-mode ()
  (require 'lalopmak-evil-org-mode)
  (defun kk/run-with-no-helm (orig-func &rest args)
    "Run a function without helm completion."
    (if (boundp 'helm-mode)
        (let ((orig-helm-mode helm-mode))
          (unwind-protect
              (progn
                (helm-mode 0)
                (apply orig-func args)
                )
            (helm-mode (if orig-helm-mode 1 0))))
      (apply orig-func args)
      ))

  (advice-add 'org-icompleting-read :around 'kk/run-with-no-helm)
  (advice-add 'org-completing-read :around 'kk/run-with-no-helm)
  (advice-add 'org-completing-read-no-i :around 'kk/run-with-no-helm)

  ; I store the specifics of my org configuration with my org files because updating projects is annoying otherwise
  (add-to-load-path "~/org/")
  (load "tag_groups")


  ; copied from Sacha Chua's config
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; next action
           "STARTED(s)"
           "WAITING(w)"
           "SOMEDAY(.)" "|" "DONE(x)" "CANCELLED(c)")
          ))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#ef2929" :weight bold))
          ("DONE" . (:foreground "#cyan-2" :weight bold))
          ("WAITING" . (:foreground "#c4a000" :weight bold))
          ("SOMEDAY" . (:foreground "gray" :weight bold))))

  (evil-leader/set-key
    "Cc" 'org-capture
    "Ca" 'org-agenda)
  )
