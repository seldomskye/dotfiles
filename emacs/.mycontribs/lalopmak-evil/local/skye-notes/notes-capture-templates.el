(defmacro buy-entry (name )
  `(,(concat "b"(downcase (substring name 0 1)))
    ,name
    entry
    (file+headline "~/org/buy_list.org" ,name)
    "* TODO %?"
    ))

(defmacro recipe-entry (name)
  `(,(concat "r"(downcase (substring name 0 1)))
    ,name
    entry
    (file+olp "~/org/zettel.org" "Cooking" ,name)
    "* %?"
    ))

(let*
    (
     (properties-source
      ":PROPERTIES:
:Source: %?
:END:
Created: %u
%t
")
     (properties
      ":PROPERTIES:
:END:
"
      )
     (title
      "* %^{Title}
")
     (title-recipe
      "* %^{Title} :recipe:
")
     (title-todo
      "* TODO %^{Title}
")
     (prompt
      "Created: %u
%t
%?
")
     (basic-note
      (concat title properties prompt))
     (basic-note-source
      (concat title properties-source))
     (recipe-note
      (concat title-recipe properties-source))
     (todo-note
      (concat title-todo properties prompt))
     (amazon (macroexpand '(buy-entry "Amazon"))
             )
     (baking (macroexpand '(recipe-entry "Baking")))
     (meal (macroexpand '(recipe-entry "Meal")))
     )
  (setq org-capture-templates
        `(("n" "Note" entry
           (file+olp "~/org/zettel.org" "Inbox")
           ,basic-note)
          ("t" "Todo" entry
           (file+olp "~/org/zettel.org" "Inbox")
           ,todo-note)
          ("v" "Weekly Review" entry
           (file+datetree "~/org/zettel.org")
           ,basic-note)
          ("w" "Work" entry
           (file+olp "~/org/zettel.org" "Work" "Hubspot")
           "* TODO %^{Title}")
          ("b" "Stuff to Buy")
          ,amazon
          ("r" "Recipes")
          ,baking
          ,meal
          )
        )
  )

(setq org-todo-keywords
      '((sequence "BLOCKED" "TODO" "DONE")))

(provide 'notes-capture-templates)
