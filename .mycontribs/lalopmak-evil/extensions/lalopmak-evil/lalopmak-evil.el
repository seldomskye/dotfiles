
;;  Colemak Evil: A set of optimized Vim-like key bindings for Emacs.
;;  Copyright (C) 2013 Patrick Brinich-Langlois

;;  lalopmak-evil: A more geometric fork.
;;  Copyright (C) 2013

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.


(require 'lalopmak-evil-base)


;;;;;;;;;;;;;;;;; Bindings ;;;;;;;;;;;;;;;;;;;



;;; Up/down/left/right
(set-in-all-evil-states-but-insert "u" 'evil-previous-line)
(set-in-all-evil-states-but-insert "e" 'evil-next-line)
(set-in-all-evil-states-but-insert "n" 'lalopmak-evil-backward-char)
(set-in-all-evil-states-but-insert "i" 'lalopmak-evil-forward-char)
;; (lalopmak-evil-define-key evil-operator-state-map "i" 'evil-forward-char)

;; ;;; Turbo navigation mode
;; (set-in-all-evil-states-but-insert "I" 'evil-backward-paragraph)
;; (set-in-all-evil-states-but-insert "N" 'evil-forward-paragraph)
;; (set-in-all-evil-states-but-insert "E" 'evil-backward-sentence)
;; (set-in-all-evil-states-but-insert "U" 'evil-forward-sentence)

;;; Beginning/end of line (home/end)
;; Use back-to-indentation instead of evil-beginning-of-line so that
;; cursor ends up at the first non-whitespace character of a line. 0
;; can be used to go to real beginning of line
(set-in-all-evil-states-but-insert "L" 'back-to-indentation)
(set-in-all-evil-states-but-insert "Y" 'evil-end-of-line)

(evil-define-motion lalopmak-evil-scroll-page-up (count)
  "Scrolls page up, centers the cursor"
  (lalopmak-evil-scroll-then-center count 'evil-scroll-page-up))

(evil-define-motion lalopmak-evil-scroll-page-down (count)
  "Scrolls page down, centers the cursor"
  (lalopmak-evil-scroll-then-center count 'evil-scroll-page-down))

;;; Page up/page down
(lalopmak-evil-define-key evil-motion-state-map "j" 'lalopmak-evil-scroll-page-up)
(lalopmak-evil-define-key evil-motion-state-map "h" 'lalopmak-evil-scroll-page-down)

;;; Page halfway up/down
(set-in-all-evil-states-but-insert "\C-u" 'evil-scroll-up)
(set-in-all-evil-states-but-insert "\C-e" 'evil-scroll-down)

;;; Words forward/backward
(set-in-all-evil-states-but-insert "l" 'evil-backward-word-begin)
(set-in-all-evil-states-but-insert "y" 'evil-forward-word-end)


;; word object maps from default
;; (lalopmak-evil-define-key evil-outer-text-objects-map "l" 'evil-a-word)
;; (lalopmak-evil-define-key evil-outer-text-objects-map "y" 'evil-a-word)
;; (lalopmak-evil-define-key evil-inner-text-objects-map "l" 'evil-inner-word)
;; (lalopmak-evil-define-key evil-inner-text-objects-map "y" 'evil-inner-word)

;;; WORD forward/backward
(set-in-all-evil-states-but-insert "\C-y" 'evil-forward-WORD-end)
(set-in-all-evil-states-but-insert "\C-l" 'evil-backward-WORD-begin)

;; WORD object maps from default
;; (lalopmak-evil-define-key evil-outer-text-objects-map "\C-l" 'evil-a-WORD)
;; (lalopmak-evil-define-key evil-outer-text-objects-map "\C-y" 'evil-a-WORD)
;; (lalopmak-evil-define-key evil-inner-text-objects-map "\C-l" 'evil-inner-WORD)
;; (lalopmak-evil-define-key evil-inner-text-objects-map "\C-y" 'evil-inner-WORD)


;;directional object maps
(lalopmak-evil-define-key evil-inner-text-objects-map "l" 'evil-inner-word)
(lalopmak-evil-define-key evil-outer-text-objects-map "l" 'evil-a-word)
(lalopmak-evil-define-key evil-inner-text-objects-map "y" 'evil-inner-WORD)
(lalopmak-evil-define-key evil-outer-text-objects-map "y" 'evil-a-WORD)
(lalopmak-evil-define-key evil-inner-text-objects-map "e" 'evil-inner-sentence)
(lalopmak-evil-define-key evil-outer-text-objects-map "e" 'evil-a-sentence)
(lalopmak-evil-define-key evil-inner-text-objects-map "u" 'evil-inner-paragraph)
(lalopmak-evil-define-key evil-outer-text-objects-map "u" 'evil-a-paragraph)

;; Not sure what this is, think it's an emacs definition.  Originally assigned to o
(lalopmak-evil-define-key evil-inner-text-objects-map "n" 'evil-inner-symbol)
(lalopmak-evil-define-key evil-outer-text-objects-map "n" 'evil-a-symbol)

;; Execute command: map : to ;
(lalopmak-evil-define-key evil-motion-state-map ";" 'evil-ex);;; End of word forward/backward

;;; Folds, etc.
;; (lalopmak-evil-define-key evil-normal-state-map ",o" 'evil-open-fold)
;; (lalopmak-evil-define-key evil-normal-state-map ",c" 'evil-close-fold)
;; (lalopmak-evil-define-key evil-normal-state-map ",a" 'evil-toggle-fold)
;; (lalopmak-evil-define-key evil-normal-state-map ",r" 'evil-open-folds)
;; (lalopmak-evil-define-key evil-normal-state-map ",m" 'evil-close-folds)

;;; I'm not sure what this is
;; for virtualedit=onemore
;; set virtualedit=block,onemore


;;; Cut/copy/paste
(set-in-all-evil-states-but-insert "x" 'evil-substitute)
(set-in-all-evil-states-but-insert "X" 'evil-delete-line)  ; delete to end of line; use dd to delete whole line
(set-in-all-evil-states-but-insert "c" 'evil-yank)
(set-in-all-evil-states-but-insert "C" 'evil-yank-line)

;; "Original" paste v/undo p
;; =====
(set-in-all-evil-states-but-insert "V" 'evil-paste-before)
(set-in-all-evil-states-but-insert "v" 'evil-paste-after)
(set-in-all-evil-states-but-insert "\C-v" 'evil-paste-pop)
(set-in-all-evil-states-but-insert "\M-v" 'evil-paste-pop-next)

;;; Undo/redo
(lalopmak-evil-define-key evil-normal-state-map "p" 'undo)
(when (fboundp 'undo-tree-undo)
  (lalopmak-evil-define-key evil-normal-state-map "p" 'undo-tree-undo)
  (lalopmak-evil-define-key evil-normal-state-map "\C-p" 'undo-tree-redo))
;; =====

;; Experimental swapped paste p/undo v
;; =====
;; (set-in-all-evil-states-but-insert "P" 'evil-paste-before)
;; (set-in-all-evil-states-but-insert "p" 'evil-paste-after)
;; (set-in-all-evil-states-but-insert "\C-p" 'evil-paste-pop)

;; ;;; Undo/redo
;; (lalopmak-evil-define-key evil-normal-state-map "v" 'undo)
;; (when (fboundp 'undo-tree-undo)
;;   (lalopmak-evil-define-key evil-normal-state-map "v" 'undo-tree-undo)
;;   (lalopmak-evil-define-key evil-normal-state-map "\C-v" 'undo-tree-redo))
;; =====



;;; Move by sentence
(set-in-all-evil-states-but-insert "N" 'evil-backward-sentence)
(set-in-all-evil-states-but-insert "I" 'evil-forward-sentence)


;;; Move cursor to top/bottom of screen
;; next/prior are page up/down
(set-in-all-evil-states (kbd "C-<next>") 'evil-window-bottom)
(set-in-all-evil-states (kbd "C-<prior>") 'evil-window-top)


;;; Make insert/add work also in visual line mode like in visual block mode
;; not sure what this means

;;; Visual mode
(set-in-all-evil-states-but-insert "a" 'evil-visual-char)
(set-in-all-evil-states-but-insert "A" 'evil-visual-line)
(set-in-all-evil-states-but-insert "\C-a" 'mark-whole-buffer)
(lalopmak-evil-define-key evil-motion-state-map "\M-a" 'evil-visual-block)


;; ;;switching sides in visual mode
;; (define-key evil-visual-state-map " a" 'exchange-point-and-mark)
;; (define-key evil-visual-state-map " A" 'evil-visual-exchange-corners)

;; ;;space-prefixed motions
;; (define-key evil-motion-state-map " " nil)
;; (define-key evil-motion-state-map "  " (lambda () (interactive) (insert " ")))
;; (define-key evil-motion-state-map " /" 'evil-scroll-line-to-bottom)
;; (define-key evil-motion-state-map " \\" 'evil-scroll-line-to-center)
;; (define-key evil-motion-state-map " !" 'evil-scroll-line-to-top)

;;;evil-surround
(setcdr evil-surround-mode-map nil) ;removes previous "s" mappings

(evil-define-key 'operator evil-surround-mode-map "z" 'surround-edit)
(evil-define-key 'visual evil-surround-mode-map "z" 'surround-region)
(evil-define-key 'visual evil-surround-mode-map "Z" 'Surround-region)

;;; visual Block mode
;; Since the system clipboard is accessible by Emacs through the
;; regular paste command (v), a separate C-v mapping isn't needed.
;; (lalopmak-evil-define-key evil-motion-state-map "\C-b" 'evil-visual-block)

;;; Allow switching from visual line to visual block mode
;; not implemented

;;; Visual mode with mouse
;; not implemented
;;; Insert literal
;; not implemented

;;; GUI search
;; not implemented

;;; Redraw screen
;; not implemented

;;; Tabs
;; Who needs tabs? Use iswitchb instead. Put (iswitchb-mode 1) in your
;; .emacs and use C-x b to search for the buffer you want. C-s and C-r
;; rotate through the listed buffers

;;; New/close/save
;; these might conflict with emacs mappings


(set-in-all-evil-states-but-insert "J" 'evil-join)


;;not motion for compatiblilty with undo-tree
(set-in-all-evil-states-but-insert-and-motion "q" 'evil-replace)
(set-in-all-evil-states-but-insert-and-motion "Q" 'evil-replace-state)

;;; Scroll in place
(lalopmak-evil-define-key evil-motion-state-map (kbd "C-<up>") 'evil-scroll-line-up)
(lalopmak-evil-define-key evil-motion-state-map (kbd "C-<down>") 'evil-scroll-line-down)

;;; Live line reordering
;; not implemented

;;; Restore mappings
;;; Free mappings: ,/+/H

;;; Macros
;; (lalopmak-evil-define-key evil-normal-state-map "Q" '(lambda ()
;; 					 (interactive)
;; 					 (evil-execute-macro 1 last-kbd-macro)))

;; (cond (window-system  ; ensure not running in a terminal
;;        (lalopmak-evil-local-set-key (kbd "<return>") 'newline)
;;        (lalopmak-evil-local-set-key "\C-m" 'evil-record-macro)))
(lalopmak-evil-define-key evil-normal-state-map "'" 'evil-execute-macro)
(lalopmak-evil-define-key evil-normal-state-map "m" 'evil-record-macro)
;; (lalopmak-evil-define-key evil-normal-state-map "\"" 'evil-execute-macro)

(lalopmak-evil-define-key evil-normal-state-map "M" 'evil-set-marker)


(define-key evil-motion-state-map (kbd "C-'") 'evil-goto-mark-line)
;;; Duplicate line
;; not implemented
;; Use "CV" instead

;;; Misc overridden keys must be prefixed with g
;; not implemented


(evil-define-operator lalopmak-evil-all-case (beg end type)
  "Converts to all case, or, if already all case, converts to all lower case."
  (let ((region (buffer-substring beg end)))
    (if (equal (upcase region)
               region)
        (evil-downcase beg end type)
      (evil-upcase beg end type))))


(lalopmak-evil-define-key evil-visual-state-map "m" 'lalopmak-evil-all-case)

;;; Search
(lalopmak-evil-define-key evil-motion-state-map "k" 'evil-search-next)
(lalopmak-evil-define-key evil-motion-state-map "K" 'evil-search-previous)

;;; Folding
;; (lalopmak-evil-define-key evil-normal-state-map "zo" 'evil-open-fold)
;; (lalopmak-evil-define-key evil-normal-state-map "zc" 'evil-close-fold)
;; (lalopmak-evil-define-key evil-normal-state-map "za" 'evil-toggle-fold)
;; (lalopmak-evil-define-key evil-normal-state-map "zr" 'evil-open-folds)
;; (lalopmak-evil-define-key evil-normal-state-map "zm" 'evil-close-folds)

;;; Window handling
;; C-w (not C-r as in Shai's mappings) prefixes window commands

(lalopmak-evil-define-key evil-window-map "n" 'evil-window-left)
(lalopmak-evil-define-key evil-window-map "N" 'evil-window-move-far-left)
(lalopmak-evil-define-key evil-window-map "e" 'evil-window-down)
(lalopmak-evil-define-key evil-window-map "E" 'evil-window-move-very-bottom)
(lalopmak-evil-define-key evil-window-map "u" 'evil-window-up)
(lalopmak-evil-define-key evil-window-map "U" 'evil-window-move-very-top)
(lalopmak-evil-define-key evil-window-map "i" 'evil-window-right)
(lalopmak-evil-define-key evil-window-map "I" 'evil-window-move-far-right)
(lalopmak-evil-define-key evil-window-map "k" 'evil-window-new)


(set-in-all-evil-states-but-insert (kbd "\\")  'evil-indent)

;;Unassigns previous object pending states
(define-key evil-visual-state-map "a" nil)
(define-key evil-visual-state-map "i" nil)
(define-key evil-operator-state-map "a" nil)
(define-key evil-operator-state-map "i" nil)


;; Insert / inner object pending state
(set-in-all-evil-states-but-insert "r" 'evil-insert)
(set-in-all-evil-states-but-insert "R" 'evil-insert-line)
(lalopmak-evil-define-key evil-operator-state-map "r" evil-inner-text-objects-map)
(lalopmak-evil-define-key evil-visual-state-map "r" evil-inner-text-objects-map)

;;Append / outer object pending state
(set-in-all-evil-states-but-insert "s" 'evil-append)
(set-in-all-evil-states-but-insert "S" 'evil-append-line)
(lalopmak-evil-define-key evil-operator-state-map "s" evil-outer-text-objects-map)
(lalopmak-evil-define-key evil-visual-state-map "s" evil-outer-text-objects-map)

;;Change
(set-in-all-evil-states-but-insert "T" 'evil-change-line)
(set-in-all-evil-states-but-insert "t" 'evil-change)

;; (set-in-all-evil-states-but-insert "p" 'evil-substitute)   ;tentative assignment
;; (set-in-all-evil-states-but-insert "\C-p" 'evil-change-whole-line)

;;conflicts with undo
;; (set-in-all-evil-states-but-insert "\C-t" 'evil-jump-backward)
;; (set-in-all-evil-states-but-insert "\C-p" 'evil-jump-forward)


;;old find char/reverse for use in macros
(set-in-all-evil-states-but-insert "\M-f" 'evil-find-char)
(set-in-all-evil-states-but-insert "\M-w" 'evil-find-char-backward)
(set-in-all-evil-states-but-insert "\M-t" 'evil-repeat-find-char)

;;Line jump
(set-in-all-evil-states-but-insert "o" 'lalopmak-evil-if-count-goto-line-else-ace-jump-line-mode) ;temporary assignment

;switch to buffer
(lalopmak-evil-define-key evil-motion-state-map "b" 'switch-to-buffer)
(lalopmak-evil-define-key evil-motion-state-map "\M-b" 'ido-write-file)
(lalopmak-evil-define-key evil-motion-state-map "\C-b" 'fiplr-find-file)
(lalopmak-evil-define-key evil-motion-state-map "B" 'find-file)


;;;;;;;;;;;;PASTING;;;;;;;;;;;;;;;;;;
(evil-define-motion lalopmak-evil-paste-below (count)
  "Pastes in the line below."
  (evil-open-below 1)
  ;; (newline count) ;;TODO count indicates number of lines until the paste
  (evil-paste-after 1))

(evil-define-motion lalopmak-evil-paste-below-then-normal (count)
  "Pastes in the line below then normal mode."
  (lalopmak-evil-paste-below count)
  (evil-normal-state))

(evil-define-motion lalopmak-evil-paste-above (count)
  "Pastes in the line above."
  (evil-open-above 1)
  ;; (newline count) ;;TODO count indicates number of lines until the paste
  (evil-paste-after 1))

(evil-define-motion lalopmak-evil-paste-above-then-normal (count)
  "Pastes in the line above then normal mode."
  (lalopmak-evil-paste-above count)
  (evil-normal-state))

(evil-define-motion lalopmak-evil-paste-at-bol (count)
  "Pastes at beginning of line."
  (back-to-indentation)
  (evil-paste-before 1))

(evil-define-motion lalopmak-evil-paste-at-eol (count)
  "Pastes at end of line."
  (evil-end-of-line)
  (evil-paste-after 1))

;;o to open in line above/below, or [number]o to go to line [number]
;; (set-in-all-evil-states-but-insert "z" 'evil-open-below)
(set-in-all-evil-states-but-insert "Z" 'evil-open-below)

;;M-[direction] to paste in that direction
(set-in-all-evil-states-but-insert "\M-u" 'lalopmak-evil-paste-above-then-normal)
(set-in-all-evil-states-but-insert "\M-e" 'lalopmak-evil-paste-below-then-normal)
(lalopmak-evil-define-key evil-insert-state-map "\M-u" 'lalopmak-evil-paste-above)
(lalopmak-evil-define-key evil-insert-state-map "\M-e" 'lalopmak-evil-paste-below)
(set-in-all-evil-states "\M-n" 'lalopmak-evil-paste-at-bol)
(set-in-all-evil-states "\M-i" 'lalopmak-evil-paste-at-eol)



(lalopmak-evil-define-key evil-motion-state-map "0" 'evil-beginning-of-line)


;;necessary or we get errors when trying to map "EE", "UE", etc
(lalopmak-evil-define-key evil-motion-state-map "E" nil)
(lalopmak-evil-define-key evil-motion-state-map "U" nil)

(lalopmak-evil-define-key evil-motion-state-map "EU" 'evil-forward-section-begin)
(lalopmak-evil-define-key evil-motion-state-map "EE" 'evil-forward-section-end)
(lalopmak-evil-define-key evil-motion-state-map "UU" 'evil-backward-section-begin)
(lalopmak-evil-define-key evil-motion-state-map "UE" 'evil-backward-section-end)
(lalopmak-evil-define-key evil-motion-state-map "U(" 'evil-previous-open-paren)
(lalopmak-evil-define-key evil-motion-state-map "U)" 'evil-previous-open-paren)
(lalopmak-evil-define-key evil-motion-state-map "E(" 'evil-next-close-paren)
(lalopmak-evil-define-key evil-motion-state-map "E)" 'evil-next-close-paren)
(lalopmak-evil-define-key evil-motion-state-map "U{" 'evil-previous-open-brace)
(lalopmak-evil-define-key evil-motion-state-map "U}" 'evil-previous-open-brace)
(lalopmak-evil-define-key evil-motion-state-map "E}" 'evil-next-close-brace)
(lalopmak-evil-define-key evil-motion-state-map "E}" 'evil-next-close-brace)

;;tentative assignment; for the key in top middle
(lalopmak-evil-define-key evil-motion-state-map "!" 'evil-jump-item)

;; Makes ; an alias for :
(set-in-all-evil-states-but-insert ";" 'evil-ex)


;;hooks for hints
(evil-ex-define-cmd "hints" 'lalopmak-evil-hints)
(evil-ex-define-cmd "ars" "hints")

(evil-ex-define-cmd "mnemonic" 'lalopmak-evil-mnemonic-hints)



;;Experiment: swaps o and :
;; (set-in-all-evil-states-but-insert ";" 'lalopmak-evil-goto-line-if-count-else-open-below)
;; (set-in-all-evil-states-but-insert ":" 'evil-open-above)
;; (set-in-all-evil-states-but-insert "o" 'evil-ex)
;; (set-in-all-evil-states-but-insert "O" 'evil-ex)

;; (evil-define-key 'normal evil-paredit-mode-map
;;   (kbd "d") 'evil-paredit-delete
;;   (kbd "t") 'evil-paredit-change
;;   (kbd "c") 'evil-paredit-yank
;;   (kbd "D") 'evil-paredit-delete-line
;;   (kbd "T") 'evil-paredit-change-line
;;   (kbd "C") 'evil-paredit-yank-line
;;   (kbd "x") 'paredit-backward-delete
;;   (kbd "X") 'paredit-forward-delete)

;;experiment
(setq evil-cross-lines t)

(provide 'lalopmak-evil)
