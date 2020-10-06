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

(defun ma-github-github-create ()
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

(defun ma-github-local-get-path (name)
  "Ask for local repository’s path, with NAME as the default dir."
  (expand-file-name
   (read-directory-name "Repository dir " name)))

(defun ma-github-get-repo-info ()
  "Ask for repository’s name and local path."
  (let ((name (read-string "Repository name: ")))
    ;; Create a list to return with repository's name and dir
    (list name (ma-github-local-get-path name))))

(defun ma-github-local-add-remote (name dir)
  "Add a Git “remote” for the local repository."
  (shell-command
   (concat "git -C " dir " remote add origin "
           "git@github.com:"
           (getenv ma-github-env-user) "/"
           name ".git")))

(defun ma-github-local-create (name dir)
  "Create a new local repository named NAME inside directory DIR."
  (interactive (ma-github-get-repo-info))
  (make-directory dir)
  (shell-command (concat "git -C " dir " init .")))

(defun ma-github-local-kickstart (dir)
  "Add a blank file inside directory DIR and commit it."
  (interactive "DRepository dir: ")
  (message dir)
  (shell-command (concat "touch " dir "/README"))
  (shell-command (concat "git -C " dir " add " dir "/README"))
  (shell-command (concat "git -C " dir " commit -m 'Initial commit'")))

(defun ma-github-local-push (dir)
  "Run a “Git-push” for the repository in directory DIR."
  (shell-command
   (concat "git -C " dir " push -u origin master")))

(defun ma-github-create (name dir kickstart git-push)
  "Create repository named NAME inside DIR, and also in Github.
If KICKSTART is t, create a blank file inside DIR, “Git-add” it,
and run a “Git-commit”. If GIT-PUSH is t, run a “Git-push” inside DIR."
  (interactive
   (nconc (ma-github-get-repo-info)
          (list (yes-or-no-p "Push?")
                (yes-or-no-p "Kickstart it (README and first commit)?"))))
  (ma-github-local-create name dir)
  (ma-github-local-add-remote name dir)
  (when git-push (ma-github-local-push dir))
  (when kickstart (ma-github-local-kickstart dir)))
