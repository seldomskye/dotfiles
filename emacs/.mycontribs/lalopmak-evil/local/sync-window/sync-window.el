;;; sync-window.el --- Synchronize two side-by-side windows

;; Copyright (C) 2013  Zawada

;; Author: Tobias Zawada <i@tn-home.de>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is experimental code.
;; The main function is sync-window-mode (which see).

;;; Code:

(setq sync-window-windows (list))

(defun sync-window-cleanup ()
  "Clean up after `sync-window-modeeq'.print
  (lue is a single function; it sets or changes the value to a list of functions.

If local is non-nil, that says to add function to the buffer-local hook list instead of to the global hook list. This makes the hooiemacs"
  (interactive)
  )

(defvar sync-window-master-hook nil
  "Hooks to be run by `sync-window' in the master window ")

(defun sync-window (&optional display-start)
  "Synchronize point position other window in current frame.
Only works if there are exactly two windows in the active wrame not counting the minibuffer."
  (interactive)
  (let ((init-window (selected-window)))
    (if (and
         (= (length sync-window-windows) 2)
         (window-live-p  (first sync-window-windows))
         (window-live-p  (second sync-window-windows)))
        (progn
          (run-hooks 'sync-window-master-hook)
          (select-window (first sync-window-windows)) ;; 1st element = gets synced
          (goto-char (window-point (second sync-window-windows)))
          (select-window init-window)
          (unless display-start
            (redisplay t))
          )
      (setq sync-window-windows (list)))))

(defvar sync-window-mode-hook nil
  "Hooks to be run at start of `sync-window-mode'.")

(defun window-sync-window-hook ()
  ""
  (when (and (= 2 (length sync-window-windows))
             (equal (selected-window) (second sync-window-windows)))
    (set-window-buffer (first sync-window-windows) (window-buffer (second sync-window-windows)))
    ))

(defun window-sync-init ()
  (interactive)
  (let* ((initial-window (selected-window))
         (new-frame (make-frame '((name . "sync-frame") (minibuffer . nil) (unsplittable t))))
         (top-window  (frame-first-window new-frame))
         (delete-if-not-top (lambda (wind) (unless (eq top-window wind)
                                             (window--delete wind)))))
    (mapc delete-if-not-top
          (window-list new-frame))
    (set-window-buffer top-window (window-buffer initial-window))
    (setq sync-window-windows (list top-window initial-window))
    )
  )

(define-minor-mode sync-window-mode 
  "Synchronized view of two buffers in two side-by-side windows."
  :group 'windows
  :lighter " ⇕"
  (if sync-window-mode
      (progn
        (add-hook 'post-command-hook 'sync-window-wrapper 'append)
        (add-to-list 'window-scroll-functions 'sync-window-wrapper)
        (add-hook 'window-configuration-change-hook 'window-sync-window-hook 'append)
        (run-hooks 'sync-window-mode-hook)
        (sync-window))
    (remove-hook 'post-command-hook 'sync-window-wrapper)
    (remove-hook 'window-configuration-change-hook 'window-sync-window-hook)
    (setq window-scroll-functions (remove 'sync-window-wrapper window-scroll-functions))
    ))

(defun sync-window-wrapper (&optional window display-start)
    "This wrapper makes sure that `sync-window' is fired from `post-command-hook'
only when the buffer of the active window is in `sync-window-mode'."
    ;; Maybe this only run if the current window is the one we're syncing to other modes
    (when
         (member (or window (selected-window)) sync-window-windows)
      (sync-window display-start)))

(provide 'sync-window)
