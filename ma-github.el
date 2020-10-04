(defun ma-github-create-github-repo ()
  "Create a new repository in github.com"
  (interactive)
  (let ((repo-name (read-string "Repository name "))
        (is-private (if (yes-or-no-p "Public") "false" "true")))
    (shell-command 
     (concat
      "curl "
      (concat "-H \"Authorization: token " (getenv "GITHUB_TOKEN") "\" ")
      "https://api.github.com/user/repos "
      (concat "-d '{\"name\":\"" repo-name "\", \"private\": " is-private "}'")))
    repo-name))
