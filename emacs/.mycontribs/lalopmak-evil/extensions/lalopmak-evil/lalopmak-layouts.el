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


;;; Keyboard Layout related functions

(defun colemak-to-qwerty () 
  "Returns the colemak to qwerty map"
  '(("f"."e") ("p"."r") ("g"."t") ("j"."y") ("l"."u") ("u"."i") ("y"."o") (";"."p") (":"."P") ;;TODO: fix bizarre error, triggered in  (kbdconcat "C-" strRest)) ;("\'"."[") ("\""."{")
    ("r"."s") ("s"."d") ("t"."f") ("d"."g") ("n"."j") ("e"."k") ("i"."l") ("o".";") ("O".":")
    ("k"."n")))

(defun colemak-to-colemak ()
  "Returns the colemak to colemak map, aka itself"
  '())

(defmacro kbdconcat (&rest args)
  "Short for (kbd (concat [args]))"
  `(kbd (concat ,@args)))

(defmacro key-equals (key &rest args)
  "Checks if key is equal to the kbdconcat of the rest"
  `(equal ,key
          (kbdconcat ,@args)))

(defun check-all-keys (key layoutMap)
  "Brute force checks and returns matchings between the key and (kbd [modifier]-[layout keys])" 
  (first (delq nil 
               (mapcar (lambda (mapping)
                         (let* ((strFirst (first mapping))
                                (kbdFirst (kbd strFirst))
                                (strRest (rest mapping))) 
                           (cond ((key-equals key 
                                              "C-" strFirst)
                                  (kbdconcat "C-" strRest))
                                 ((key-equals key 
                                              "M-" strFirst)
                                  (kbdconcat "M-" strRest))
                                 (t nil))))
                       layoutMap))))


(defun key-to-layout (key layout)
  "Given a key and a layout map, gets the corresponding key in the other layout.  Preferred input via strings, e.g. '\C-a'"
  (let ((layoutMap (funcall layout)))
    (if layoutMap    ;only do anything if the layout is non-trivial
        (if (stringp key)   
            (let ((result (assoc key layoutMap))                  ;result of the layout map
                  (lowerResult (assoc (downcase key) layoutMap))  ;result of lowercase
                  (upperResult (assoc (upcase key) layoutMap))    ;result of uppercase
                  (hyphen (string-match "-" key)))
              (cond (result (rest result))  ;if it's in our layoutMap map, we take it
                    (hyphen (let ((prefix (substring key 0 hyphen)) ;if we have hyphen, eg M-x, to M-(key-to-layoutMap x)
                                  (suffix (substring key (1+ hyphen))))
                              (concat prefix "-" (key-to-layout suffix layout))))  
                    (lowerResult (upcase (rest lowerResult)))   ;key originally uppercase
                    (upperResult (downcase (rest upperResult)))  ;key originally lowercase
                    (t key))) ;returns itself as fallback
          (let ((completeCheck (check-all-keys key layoutMap)))  ;brute force check
            (if completeCheck completeCheck key)))  
      key)))

(provide 'lalopmak-layouts)
