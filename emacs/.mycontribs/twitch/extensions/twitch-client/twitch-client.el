(require 'request)

(defun twitch-handle-response (response)
  (message "%s" response)
  (if (not  (equal (length response) 0))
      (let*
          ((cands (twitch-get-channelname-url response)))
        (helm :sources
              `((name . "Twitch Followed Streams")
                (candidates . ,cands)
                (action . run-livestreamer)))
      )))

(defun twitch-get-channelname-url (response)
  (let*
      ((streams (cdr  (assq 'streams response)))
       (channels  (map 'list
                       (lambda (x) (cdr  (assq 'channel x)))
                       streams))
       (urls (map 'list (lambda (channel) (cdr (assq 'url channel))) channels))
       (display-names (map 'list (lambda (channel) (cdr (assq 'display_name channel))) channels)))
    (pairlis display-names urls)
    ))

(defun run-livestreamer (thing)
  (call-process-shell-command (concat  "livestreamer " thing " best") nil 0))

(defvar twitch-auth-key "osre2t8rjs9kdv4p0dxmvykpzrdvlc")

(defun open-twitch-streams ()
  (interactive)
  (request 
   "https://api.twitch.tv/kraken/streams/followed"
   :headers `(("Authorization" . ,(concat "OAuth " twitch-auth-key)))
   :parser 'json-read
   :success (function* (lambda (&key data &allow-other-keys)
                         (twitch-handle-response data))
                       )))

(provide 'twitch-client)
