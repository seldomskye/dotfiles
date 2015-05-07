(defvar themes-packages 
  '(
    moe-theme
)
)

(defun themes/init-moe-theme ()
  (use-package moe-theme
   :config
   (moe-light)))
