  (setq org-capture-templates
        `(("n" "Note" entry
           (file+olp "~/Dropbox (HubSpot)/work.org" "Inbox")
           )
          ("t" "todos")
          ("tw" "Work Todo" entry
           (file+olp "~/Dropbox (HubSpot)/work.org" "Inbox")
           "* TODO %?\n")
          ("tp" "Personal Todo" entry
           (file+olp "~/org/zettel.org" "Inbox")
           "* TODO %?\n%U\n%a\n"
           )
          ("g" "Git Issue" entry
           (file+olp "~/Dropbox (HubSpot)/work.org" "Dev Projects")
           "* TODO [[https://git.hubteam.com/HubSpot/AutomationTeam/issues/%\\1][Issue %^{Issue Number?}]]\n"
           )
          ("j" "Jira Issue" entry
           (file+olp "~/Dropbox (HubSpot)/work.org" "Jira Tickets")
           "* TODO [[https://issues.hubspotcentral.com/browse/WORKFLOWS-%\\1][Ticket %^{Ticket Number?}]]\n"
           )
          ("p" "Personal Programming" entry
           (file+olp "~/org/zettel.org" "Inbox")
           "* %^{Title}\n"
           )
          
          )
        )

(provide 'notes-capture-templates)
