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

(defun set-syncing-windows ()
  (interactive)
  (let ((num-windows (length sync-window-windows))
        (current-window (selected-window)))
    (cond
     ((= 0 num-windows) (setq sync-window-windows (list current-window)))
     ((<= 2 num-windows) (setq sync-window-windows (list)))
     ((= 1 num-windows) (setq sync-window-windows (cons current-window sync-window-windows)))
     )
    )
  )

(defun sync-window (&optional display-start)
  "Synchronize point position other window in current frame.
Only works if there are exactly two windows in the active wrame not counting the minibuffer."

  (interactive)
  (if (and
       (= (length sync-window-windows) 2) ; Need a liveness check
       (window-live-p  (first sync-window-windows))
       (window-live-p  (second sync-window-windows)))
      (let ((p (line-number-at-pos))
            (start (line-number-at-pos (or display-start (window-start))))
            (vscroll (window-vscroll))
            breg ereg)
        (when (use-region-p)
          (setq breg (line-number-at-pos (region-beginning))
                ereg  (line-number-at-pos (if (looking-back "\n") (1- (region-end)) (region-end)))))
        (run-hooks 'sync-window-master-hook)
        (select-window (first sync-window-windows)) ;; 1st element = gets synced
        (goto-char (point-min))
        (when breg
          (sync-window-cleanup)
        (setq start (line-beginning-position start))
        (forward-line (1- p))
        (set-window-start (selected-window) start)
        (set-window-vscroll (selected-window) vscroll)
        (select-window (second sync-window-windows)) ;; 2nd element = sync with
        (unless display-start
          (redisplay t))
        )
    (setq sync-window-windows (list))))

(defvar sync-window-mode-hook nil
  "Hooks to be run at start of `sync-window-mode'.")

(defun window-sync-same-buffer ()
  (interactive)
  (set-window-buffer (first sync-window-windows) (window-buffer (second sync-window-windows))))

(defun window-sync-window-hook ()
  ""
  (when (and (= 2 (length sync-window-windows))
             (equal (selected-window) (second sync-window-windows)))
    (set-window-buffer (first sync-window-windows) (window-buffer (second sync-window-windows)))
    ))

(define-minor-mode sync-window-mode ; Need to change this to be global rather than buffer local
  "Synchronized view of two buffers in two side-by-side windows."
  :group 'windows
  :lighter " ⇕"
  (if sync-window-mode
      (progn
        (add-hook 'post-command-hook 'sync-window-wrapper 'append) ; this is bad because this is actually, adding to the buffer local list, but we would've expected..window local
        (add-to-list 'window-scroll-functions 'sync-window-wrapper)
        (add-hook 'window-configuration-change-hook 'window-sync-window-hook)
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
        (member (selected-window) sync-window-windows)
      (sync-window display-start)))

(provide 'sync-window)
