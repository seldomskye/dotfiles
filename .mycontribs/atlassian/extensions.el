(defvar atlassian-post-extensions
  '(
    org-confluence
  )
)
(defun atlassian/init-org-confluence()
  (use-package org-confluence
    :init
    (progn
     (require 'ox-confluence) )
    )
  )
