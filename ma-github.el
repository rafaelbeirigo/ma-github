(defgroup ma-github nil
  "Interact with Github.")

(defcustom ma-github-repo-url "https://api.github.com/user/repos"
  "URL address used to create a new repository."
  :type 'string
  :group 'ma-github)

(defcustom ma-github-env-token "MA_GITHUB_TOKEN"
  "Environment variable containing the access token."
  :type 'string
  :group 'ma-github)

(defcustom ma-github-env-user "MA_GITHUB_USER"
  "Environment variable containing the username."
  :type 'string
  :group 'ma-github)

(defun ma-github-create-github-repo ()
  "Create a repository in Github."
  (interactive)
  (let ((repo-name (read-string "Repository name "))
        (is-private (if (yes-or-no-p "Public") "false" "true")))
    (shell-command 
     (concat
      "curl "
      (concat "-H \"Authorization: token " (getenv ma-github-env-token) "\" ")
      ma-github-repo-url " "
      (concat "-d '{\"name\":\"" repo-name "\", \"private\": " is-private "}'")))
    repo-name))

(defun ma-github-get-local-path (repo-name)
  "Ask for repository’s path, using REPO-NAME as default dir."
  (expand-file-name
   (read-directory-name "Repository dir " repo-name)))

(defun ma-github-get-repo-name-and-dir ()
  "Asks for the repository name and local dir."
  (let ((repo-name (read-string "Repository name: ")))
    ;; Create a list to return with repository's name and dir
    (list repo-name (ma-github-get-local-path repo-name))))

(defun ma-github-local-repo-add-remote (repo-name repo-dir)
  "Add a `remote' for the local repository."
  (shell-command
   (concat "git -C " repo-dir " remote add origin "
           "git@github.com:"
           (getenv ma-github-env-user) "/"
           repo-name ".git")))

(defun ma-github-create-local-repo (repo-name repo-dir)
  "Create a new repository locally"
  (interactive (ma-github-get-repo-name-and-dir))
  (make-directory repo-dir)
  (shell-command (concat "git -C " repo-dir " init .")))

(defun ma-github-local-repo-kickstart (repo-dir)
  "Add a README file and do the initial commit on the local repository."
  (interactive "DRepository dir: ")
  (message repo-dir)
  (shell-command (concat "touch " repo-dir "/README"))
  (shell-command (concat "git -C " repo-dir " add " repo-dir "/README"))
  (shell-command (concat "git -C " repo-dir " commit -m 'Initial commit'")))

(defun ma-github-local-repo-push (repo-dir)
  "Do an upstream push on the local repository."
  (shell-command
   (concat "git -C " repo-dir " push -u origin master")))

(defun ma-github-create-repo (repo-name repo-dir do-push do-kickstart)
  "Create a new repository both locally and in github.com"
  (interactive
   (nconc (ma-github-get-repo-name-and-dir)
          (list (yes-or-no-p "Push?")
                (yes-or-no-p "Kickstart it (README and first commit)?"))))
  (ma-github-create-local-repo repo-name repo-dir)
  (ma-github-local-repo-add-remote repo-name repo-dir)
  (when do-push (ma-github-local-repo-push-upstream repo-dir))
  (when do-kickstart (ma-github-local-repo-kickstart repo-dir)))
