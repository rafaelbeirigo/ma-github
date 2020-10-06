(defgroup ma-github nil
  "Interact with Github.")

(defcustom ma-github-url "https://api.github.com/user/repos"
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
  (let ((name (read-string "Repository name "))
        (is-private (if (yes-or-no-p "Public") "false" "true")))
    (shell-command 
     (concat
      "curl "
      (concat "-H \"Authorization: token " (getenv ma-github-env-token) "\" ")
      ma-github-url " "
      (concat "-d '{\"name\":\"" name "\", \"private\": " is-private "}'")))
    name))

(defun ma-github-get-local-path (name)
  "Ask for local repository’s path, with NAME as the default dir."
  (expand-file-name
   (read-directory-name "Repository dir " name)))

(defun ma-github-get-name-and-dir ()
  "Ask for repository’s name and local path."
  (let ((name (read-string "Repository name: ")))
    ;; Create a list to return with repository's name and dir
    (list name (ma-github-get-local-path name))))

(defun ma-github-local-add-remote (name dir)
  "Add a Git “remote” for the local repository."
  (shell-command
   (concat "git -C " dir " remote add origin "
           "git@github.com:"
           (getenv ma-github-env-user) "/"
           name ".git")))

(defun ma-github-create-local-repo (name dir)
  "Create a new local repository named NAME inside directory DIR."
  (interactive (ma-github-get-name-and-dir))
  (make-directory dir)
  (shell-command (concat "git -C " dir " init .")))

(defun ma-github-local-kickstart (dir)
  "Add a README file and do the initial commit on the local repository."
  (interactive "DRepository dir: ")
  (message dir)
  (shell-command (concat "touch " dir "/README"))
  (shell-command (concat "git -C " dir " add " dir "/README"))
  (shell-command (concat "git -C " dir " commit -m 'Initial commit'")))

(defun ma-github-local-push (dir)
  "Do an upstream push on the local repository."
  (shell-command
   (concat "git -C " dir " push -u origin master")))

(defun ma-github-create-repo (name dir do-push do-kickstart)
  "Create a new repository both locally and in github.com"
  (interactive
   (nconc (ma-github-get-name-and-dir)
          (list (yes-or-no-p "Push?")
                (yes-or-no-p "Kickstart it (README and first commit)?"))))
  (ma-github-create-local-repo name dir)
  (ma-github-local-add-remote name dir)
  (when do-push (ma-github-local-push-upstream dir))
  (when do-kickstart (ma-github-local-kickstart dir)))
