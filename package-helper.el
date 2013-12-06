(require 'package)

(package-initialize)
(package-refresh-contents)

(defun install-packages (packages)
  (mapc (lambda (package)
         (unless (package-installed-p package)
           (package-install package)))
  packages))

(provide 'package-helper)
