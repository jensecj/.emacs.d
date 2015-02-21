(require 'el-get)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get/")
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
(el-get 'sync)

;; install proofgeneral for coq
(el-get-bundle ProofGeneral
  (add-hook 'coq-mode-hook
            (lambda ()
              (custom-theme-set-faces
               'zenburn
               `(coq-solve-tactics-face ((t (:foreground "#BC8383"))))
               )
              (setq proof-three-window-mode-policy 'hybrid)
              )))

(provide 'init-el-get)
