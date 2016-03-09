
;;(defconst twitch-client-id "70f0s15uym9ge9x8vbjtsbu5q03oq4m")
;; (defconst twitch-client-api "https://api.twitch.tv/kraken/oauth2/authorize")
;; https://api.twitch.tv/kraken/oauth2/authorize?response_type=token&client_id=70f0s15uym9ge9x8vbjtsbu5q03oq4m&redirect_uri=http://localhost&scope=user_read

;; (defconst access_token "osre2t8rjs9kdv4p0dxmvykpzrdvlc")

;; ;; Authorize me
;; (request
;;  "https://api.twitch.tv/kraken/oauth2/authorize"
;;  :params '(("response_type" . "token")
;;            ("client_id" . "70f0s15uym9ge9x8vbjtsbu5q03oq4m")
;;            ("redirect_uri" . "http://localhost")
;;            ("scope" . "user_read")
;;            )
;;  :parser 'json-read
;;  :success (function* (lambda () (message "whoray"))))

;; (request 
;;  "https://api.twitch.tv/kraken/streams/followed/"
;;  :headers '(("Authorization" . "OAuth osre2t8rjs9kdv4p0dxmvykpzrdvlc"))
;;  :parser 'json-read
;;  :success (function* (lambda () (message "whoray"))))

;; (request 
;;  "https://api.twitch.tv/kraken/streams/followed"
;;  :headers '(("Authorization" . "OAuth osre2t8rjs9kdv4p0dxmvykpzrdvlc"))
;;  :parser 'json-read
;;  :success (function* (lambda (&key data &allow-other-keys)
;;                        (defvar testerino data)
;;                        (message "%d" (assq 'streams data)) 
;;                        (message "%s" (map 'list (lambda (x) (assq 'channel x))(assq 'streams data)))
;;                        (message "%s" (map 'list (lambda (x) (assq 'name x)) (assq 'streams data)))
;;                        (message "test")
;;                        (insert data))))

(defun twitch-handle-response (response)
  (if (not  (equal (length response) 0))
      (let*
          ((channels (map 'list
                          (lambda (x) (cdr  (rassq 'channel x)))
                          (cdr  (assq 'streams testerino))))
           (cands (map 'list (lambda (x) (cdr (assq 'name x))) channels)))
      (helm :sources
            `((name . "Twitch Followed Streams")
              (candidates . cands)
              (action . run-livestreamer))) 
      )))
(message "%s" (map 'list (lambda (x) (cdr (assq 'name x)))
                   (map 'list
                        (lambda (x) (cdr (assq 'channel x)))
                        (cdr  (assq 'streams testerino)))))

;; (request
;;  "http://httpbin.org/get"
;;  :params '(("key" . "value") ("key2" . "value2"))
;;  :parser 'json-read
;;  :success (function*
;;            (lambda (&key data &allow-other-keys)
;;              (message "I sent: %S" (assoc-default 'args data)))))

;; note that this isn't what I should be doing
;; should build the url and give me the link to click on it
;; store the secret
;; Then need to get the user to give me the access_token
;; which can be used to have me be authorized to do whatever 
