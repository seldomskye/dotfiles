;;; evil-org-mode.el --- evil keybindings for org-mode 

;; Note: this version is modified to be more like lalopmak bindings rather than standard evil

;; Copyright (C) 2012-2013 by Edward Tjörnhammar
;; Author: Edward Tjörnhammar
;; URL: https://github.com/edwtjo/evil-org-mode.git
;; Git-Repository; git://github.com/edwtjo/evil-org-mode.git
;; Created: 2012-06-14
;; Version: 0.1.0
;; Package-Requires: ((evil "0"))
;; Keywords: evil vim-emulation org-mode key-bindings presets

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Known Bugs:
;; See, https://github.com/edwtjo/evil-org-mode/issues

(require 'evil)
(require 'org)

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

(defun always-insert-item ()
  "Force insertion of org item"
  (if (not (org-in-item-p))
      (insert "\n- ")
    (org-insert-item))
  )

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function"
  (end-of-line)
  (funcall fun)
  (evil-append nil)
  )

;; normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  "gn" 'outline-up-heading
  "ge" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
         'org-forward-same-level
         'org-forward-heading-same-level)
  "gu" (if (fboundp 'org-backward-same-level)
         'org-backward-same-level
         'org-backward-heading-same-level)
  "gi" 'outline-next-visible-heading
  "t" 'org-todo
  "T" '(lambda () (interactive) (evil-org-eol-call '(org-insert-todo-heading nil)))
  "L" 'org-beginning-of-line
  "Y" 'org-end-of-line
  ";t" 'org-show-todo-tree
;  "o" '(lambda () (interactive) (evil-org-eol-call 'always-insert-item))
;  "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "<" 'org-metaleft
  ">" 'org-metaright
  ";a" 'org-agenda
  "-" 'org-cycle-list-bullet
  (kbd "TAB") 'org-cycle)

;; normal & insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state evil-org-mode-map
          (kbd "M-i") 'org-metaright
          (kbd "M-n") 'org-metaleft
          (kbd "M-u") 'org-metaup
          (kbd "M-e") 'org-metadown
          (kbd "M-I") 'org-shiftmetaright
          (kbd "M-N") 'org-shiftmetaleft
          (kbd "M-U") 'org-shiftmetaup
          (kbd "M-E") 'org-shiftmetadown
          (kbd "M-o") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-heading)
                             (org-metaright))))
          (kbd "M-t") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-todo-heading nil)
                             (org-metaright))))
          ))
      '(normal insert))

(provide 'lalopmak-evil-org-mode)
;;; evil-org.el ends here

