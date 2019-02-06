(require 'org)
(require 'ox-publish)

(defun blog-reload-firefox ()
  "Reaload visible firefox browser using `xdotool'"
  (interactive)
  ;; TODO: should be more specific, maybe using window id?
  (let ((current-window (s-trim (shell-command-to-string "xdotool getactivewindow"))))
    (shell-command "xdotool search --onlyvisible --classname Navigator windowactivate --sync key F5")
    (shell-command (format "xdotool windowactivate %s" current-window))))

(defun blog--header ()
  ""
  "<header>
    <nav>
     <ul>
      <li><a href=\"/\"> index </a></li>
      <li><a href=\"/pages/about.html\"> about </a></li>
     </ul>
   </nav>
</header>")


(defun blog--footer ()
  ""
  "<footer></footer>")


(defun blog-publish (force)
  "Publish `blog' target using `org-publish'.
Force publish all files if called with `prefix-argument'."
  (interactive "P")
  (let ((org-html-head (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"/res/css/style.css\" />\n"
                               "<link rel=\"stylesheet\" type=\"text/css\" href=\"/res/css/theme.css\" />"))

        (org-html-home/up-format (blog--header))
        (org-html-doctype "html5")
        (org-html-htmlize-output-type 'css)
        (org-html-html5-fancy 't)

        (org-html-link-home "/")
        (org-html-link-up "/")

        (org-html-preamble nil)
        (org-html-postamble nil)

        (org-export-with-toc nil)
        (org-export-with-author t)
        (org-export-with-email nil)
        (org-export-with-creator nil)
        (org-export-with-section-numbers nil)

        (org-confirm-babel-evaluate nil)
        (org-export-babel-evaluate t)
        (org-export-use-babel t)

        (org-publish-project-alist
         '(("blog-org-to-html"
            :recursive t
            :base-directory "~/vault/blog/src/blog/"
            :base-extension "org"
            :publishing-directory "~/vault/blog/"
            :publishing-function org-html-publish-to-html
            :headline-levels 4)
           ("blog-copy-statics"
            :recursive t
            :base-directory "~/vault/blog/src/blog/"
            :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
            :publishing-directory "~/vault/blog/"
            :publishing-function org-publish-attachment)
           ("blog" :components ("blog-org-to-html" "blog-copy-statics"))
           )))
    (org-publish-project "blog" force)
    (blog-reload-firefox)))

(provide 'blog)
