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

(defun ma-github-get-repo-dir (repo-name)
  "Asks for the repository local dir."
  (expand-file-name
   (read-directory-name "Repository dir " repo-name)))

(defun ma-github-get-repo-name-and-dir ()
  "Asks for the repository name and local dir."
  (let ((repo-name (read-string "Repository name: ")))
    ;; Create a list to return with repository's name and dir
    (list repo-name (ma-github-get-repo-dir repo-name))))

(defun ma-github-local-repo-add-remote (repo-name repo-dir)
  "Add a `remote' for the local repository."
  (shell-command
   (concat "git -C " repo-dir " remote add origin "
           "git@github.com:"
           (getenv "GITHUB_USER") "/"
           repo-name ".git")))

(defun ma-github-create-local-repo (repo-name repo-dir)
  "Create a new repository locally"
  (interactive (ma-github-get-repo-name-and-dir))
  (make-directory repo-dir)
  (shell-command (concat "git -C " repo-dir " init .")))

(defun ma-github-local-repo-kickstart (repo-dir commit)
  "Do a kickstart on the local repository"
  (interactive (list (read-directory-name "Repository dir: ")
                     (yes-or-no-p "Push?")))
  (message repo-dir)
  (shell-command (concat "touch " repo-dir "/README"))
  (shell-command (concat "git -C " repo-dir " add " repo-dir "/README"))
  (shell-command (concat "git -C " repo-dir " commit -m 'Initial commit'"))
  (when commit
    (shell-command (concat "git -C " repo-dir " push -u origin master"))))

(defun ma-github-create-repo ()
(defun ma-github-local-repo-push (repo-dir)
  "Do an upstream push on the local repository."
  (shell-command
   (concat "git -C " repo-dir " push -u origin master")))

  "Create a new repository both locally and in github.com"
  (interactive)
  (let ((repo-name (ma-github-create-github-repo)))
    (let ((repo-dir (ma-github-create-local-repo repo-name)))
      (message (concat "esse: " repo-dir))
      (ma-github-kickstart-local-repo repo-dir (yes-or-no-p "Push?")))))
