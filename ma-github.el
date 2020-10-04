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

(defun ma-github-create-local-repo-dir (repo-name)
  "Create a new folder for the repository"
  (interactive)
  (let ((newdir (read-directory-name "Path for the new repository " repo-name)))
    (if (not (file-directory-p newdir))
        (make-directory newdir))
    newdir))

(defun ma-github-create-local-repo (repo-name)
  "Create a new repository locally"
  (interactive)
  (let ((repo-dir (ma-github-create-local-repo-dir repo-name)))
    (shell-command (concat "git -C " repo-dir " init ."))
    (shell-command (concat "git -C " repo-dir " remote add origin "
                           "git@github.com:"
                           (getenv "GITHUB_USER") "/"
                           repo-name ".git"))
    repo-dir))
