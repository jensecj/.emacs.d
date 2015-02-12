
;; testing some el-get
(require 'el-get)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
(el-get 'sync)

;; install proofgeneral for coq
(el-get-bundle ProofGeneral)

(provide 'init-experimental)
