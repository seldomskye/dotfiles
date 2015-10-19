;;; License

;; This software is licensed under the CC0 1.0 Public Domain Declaration, as
;; released by Creative Commons <http://creativecommons.org/publicdomain/zero/1.0/>.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS",
;; WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
;; THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;Buffer helper commands/macros


(defmacro if-visible-buffer (bufferName then-stmt &rest else-stmts)
  "If buffer by that name is visible, then-statement, else, else-stmts.
Returns the value of then-stmt or else the last of the else-stmts
then-statement must be one expression, but else-stmts can be zero or more expressions.
If the buffer is not visible, and there are no else-stmts, the value is nil."
  (declare (indent 2))
  `(let ((buffer (get-buffer ,bufferName)))
     (if (and buffer
              (get-buffer-window buffer))
         ,then-stmt
       ,@else-stmts)))

;;Pattern: Closing visible thing or else doing something (most likely starting up one)

(defmacro close-visible-buffer-else-do (bufferName &rest else-stmts)
  "If buffer by that name is visible, close it and kill the buffer.  Otherwise, execute else-stmts."
  (declare (indent defun))
  `(if-visible-buffer ,bufferName
       (progn (delete-windows-on ,bufferName)
              (kill-buffer ,bufferName))
     ,@else-stmts))

(defmacro close-visible-window-else-do (bufferName &rest else-stmts)
  "If buffer by that name is visible, close it.  Otherwise, execute else-stmts."
  (declare (indent defun))
  `(if-visible-buffer ,bufferName
       (delete-windows-on ,bufferName)
     ,@else-stmts))

;;Helper functions/macros

(defmacro do-in-new-buffer (bufferName &rest else-stmts)
  "Splits current window, call it bufferName (or unique variant thereof), execute else-stmts in the buffer"
  (declare (indent 1))
  `(progn (split-window-sensibly (selected-window))
          (other-window 1)
          ,@else-stmts
          (rename-buffer ,bufferName 'make-unique)))

(defmacro do-in-buffer (bufferName &rest else-stmts)
  "If the buffer already exists, open it up in a window.  Otherwise, execute else-stmts in new buffer."
  (declare (indent 1))
  `(if (get-buffer ,bufferName)
       (switch-to-buffer-other-window ,bufferName)
     (do-in-new-buffer ,bufferName ,@else-stmts)))

(defun do-func-in-new-buffer (bufferName func)
  "Splits current window, call it bufferName (or unique variant thereof), execute func in the buffer"
  (do-in-new-buffer bufferName (funcall func)))

(defun do-func-in-buffer (bufferName func)
  "If the buffer already exists, open it up in a window.  Otherwise, execute func in new buffer."
  (do-in-buffer bufferName (funcall func)))
;;;;;;;;;;;;;
;;Pattern: closing visible then calling a helper on the same name
;;These need to go BELOW the helpers above, or else they won't be recognized

(defmacro close-visible-buffer-else-call-helper (bufferName helper &rest call-args)
  "Closes the visible window or, if closed, calls the helper function/macro with those call-args"
  (declare (indent defun))
  `(close-visible-buffer-else-do ,bufferName
     (,helper ,bufferName ,@call-args)))


(defmacro close-visible-window-else-call-helper (bufferName helper &rest call-args)
  "Closes the visible window or, if closed, calls the helper function/macro with those call-args"
  (declare (indent defun))
  `(close-visible-window-else-do ,bufferName
     (,helper ,bufferName ,@call-args)))

;;;;;;;;;;;;;

(provide 'lalopmak-buffer)
