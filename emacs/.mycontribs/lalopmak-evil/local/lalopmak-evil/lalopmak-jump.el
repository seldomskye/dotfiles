
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


(require 'lalopmak-jump-libraries)

;;If the user wants to set a default number of lines to search for jumps
(defvar lalopmak-evil-ace-jump-num-lines nil)

(defun lalopmak-evil-set-ace-jump-num-lines (n)
  (if (>= n 0)
      (setq lalopmak-evil-ace-jump-num-lines n)
    (setq lalopmak-evil-ace-jump-num-lines nil)))

;; Sets number of lines to search in an ace jump
(evil-ex-define-cmd "acejumplines" (lambda (n) 
                                     (interactive "n# lines to search in ace jump (negative to unset): ")
                                     (lalopmak-evil-set-ace-jump-num-lines n)))

(defvar ace-query "Query Char:"
  "The query that ace-jump gives us")

;;max lines, words, chars to search
(defvar ace-jump-max-lines 20
  "Max number of lines we search through for counting jumps.")
(defvar ace-jump-max-words 1000
  "Max number of word regions we search through for counting jumps.")
(defvar ace-jump-max-chars 1000
  "Max number of character regions we search through for counting jumps.")

(defvar jump-word-search-threshold 3000
  "Number of characters on the screen before we switch to word search;
should depend on ace-jump-max-chars.")

(defvar lalopmak-jump-timing t
  "Whether or not we want to time our ace-jumps")

(defmacro with-stopwatch-if-timing (message &rest body)
  "Only calls with-stopwatch macro if it exists and we're timing.
message and body as in with-stopwatch." 
  `(if (and lalopmak-jump-timing
            (fboundp 'with-stopwatch))
       (with-stopwatch ,message ,@body)      
     ,@body))

(evil-define-motion lalopmak-evil-ace-jump-line-mode (count)
  (with-stopwatch-if-timing "Ace Line Jump"
                            (evil-ace-jump-line-mode count)))

(defmacro max-regions-for-one-ace-jump (char region-restrictor regions-search-limit)
  "Max number of lines around cursor for which we can limit an ace jump of char so that it completes in a single step.
Limited by ace-jump-max-lines or regions-search-limit, our search bound."
  `(max-regions-for-one-jump ,char ,region-restrictor ,regions-search-limit (length ace-jump-mode-move-keys)))


;;;
;;; normal jump mode
;;;

(defmacro ace-jump-char-within-n-regions (char region-restrictor &optional n)
  "Calls ace-jump-char on char, limiting possible results to within n (default 0) lines of the pointer."
  `(let ((ace-jump-mode-scope 'window))        ;;makes sure we don't leak to other scopes
     (,region-restrictor (or ,n 0) (ace-jump-char-mode ,char))))
                                  
(defmacro lalopmak-evil-ace-char-jump-mode-for-region (count region-restrictor max-regions)
  "Ace-jumps within the largest region where you would result in a single ace-search."
  `(let* ((char (get-user-input-character ace-query))
          (numRegions (or ,count 
                          lalopmak-evil-ace-jump-num-lines
                          (max-regions-for-one-ace-jump char
                                                        ,region-restrictor
                                                        ,max-regions))))
       (evil-enclose-ace-jump-for-motion 
        (ace-jump-char-within-n-regions char ,region-restrictor numRegions)))) 

(evil-define-motion lalopmak-evil-narrowed-ace-jump-char-mode (count)
  "Ace jumps within count lines, or according to user-set lalopmak-evil-ace-jump-num-lines, or the most of region that would result in a single ace-search"
  :type inclusive
  :repeat abort
  ;;Three possible search regions so far: chars, words, lines, in increasing granuity.
  (with-stopwatch-if-timing "Ace-jump one-step"
    (cond ((or count 
               lalopmak-evil-ace-jump-num-lines) 
           ;;if user provided restriction input we assume it's in lines
           (lalopmak-evil-ace-char-jump-mode-for-region count do-within-n-lines ace-jump-max-lines))
          ((< (chars-in-window) jump-word-search-threshold)
           ;;there are few enough characters for a char search to cover it
           (lalopmak-evil-ace-char-jump-mode-for-region count do-within-n-chars ace-jump-max-chars))
          ;;there are too many characters, default to word search to cover more area
          (t (lalopmak-evil-ace-char-jump-mode-for-region count do-within-n-words ace-jump-max-words)))))


(evil-define-motion lalopmak-evil-ace-jump-char-mode (count)
  "Jump visually directly to a char using ace-jump.  Has stopwatch and exclusive."
  :type inclusive
  (with-stopwatch-if-timing "Ace-jump"
                            (evil-without-repeat 
                              (evil-enclose-ace-jump-for-motion 
                                (call-interactively #'ace-jump-char-mode)))))

;;;
;;; "jump-to" mode
;;;

;;Corrects repositories; might not be needed if fixed
(evil-define-motion lalopmak-evil-ace-jump-char-to-mode (count)
  "Ace jumps within count lines, or default.  Stops one character short of result."
  :type inclusive
  :repeat abort
  (search-to-searchTo (lalopmak-evil-ace-jump-char-mode count)))
 
(evil-define-motion lalopmak-evil-narrowed-ace-jump-char-to-mode (count)
  "Ace jumps within count lines, or default.  Stops one character short of result."
  :type inclusive
  :repeat abort
  (search-to-searchTo (lalopmak-evil-narrowed-ace-jump-char-mode count)))


(provide 'lalopmak-jump)
