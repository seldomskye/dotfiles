(setq twitch-packages
      '(
        (request-deffered)
        (twitch-client :location local)
                ))


(defun twitch/init-twitch-client ()
  (use-package twitch-client
    :init
    (progn
       (spacemacs/set-leader-keys
         "ot" 'open-twitch-streams
         )
       ) 
    ))


