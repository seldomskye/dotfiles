  (setq org-capture-templates
        `(("n" "Note" entry
           (file+olp "~/Dropbox (HubSpot)/work.org" "Inbox")
           )
          ("t" "Todo" entry
           (file+olp "~/Dropbox (HubSpot)/work.org" "Inbox")
           "* TODO %?\n")
          ("g" "Git Issue" entry
           (file+olp "~/Dropbox (HubSpot)/work.org" "Dev Projects")
           "* TODO [[https://git.hubteam.com/HubSpot/AutomationTeam/issues/%\\1][Issue %^{Issue Number?}]]\n"
           )
          ("j" "Jira Issue" entry
           (file+olp "~/Dropbox (HubSpot)/work.org" "Jira Tickets")
           "* TODO [[https://issues.hubspotcentral.com/browse/WORKFLOWS-%\\1][Ticket %^{Ticket Number?}]]\n"
           )
          )
        
        )

(setq org-todo-keywords
      '((sequence "BLOCKED" "TODO" "DONE")))

(provide 'notes-capture-templates)
