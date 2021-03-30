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

(defvar ma-github-last-repo-name nil
  "Name of the last repository created.")

(defvar ma-github-last-repo-dir nil
  "Directory of the last repository created.")

(defvar ma-github-create-finalize-hook nil
  "Hook run just before the end of the ‘ma-github-create’.")

(defun ma-github-repo-ask-name ()
  "Ask for repository’s name."
  (read-string "Repository name: "))

(defun ma-github-github-ask-token ()
  "Ask for the Github access token."
  (let ((input (read-string
                (concat "Github access token (default "
                        ma-github-env-token "): "))))
    (if (string= "" input)
        (getenv ma-github-env-token)
      input)))

(defun ma-github-github-create (name token private)
  "Create a repository named NAME on Github using access token TOKEN.
The repository will be created “public” unless PRIVATE is non-nil."
  (interactive (list (ma-github-repo-ask-name)
                     (ma-github-github-ask-token)
                     (not (yes-or-no-p "Public? "))))
  (shell-command
   (concat
    "curl "
    (concat "-H \"Authorization: token " token "\" ")
    ma-github-url " "
    (concat "-d '{\"name\":\"" name "\", "
            "\"private\": " (if private "true" "false" ) "}'"))))

(defun ma-github-local-ask-path (name)
  "Ask for local repository’s path, with NAME as the default dir."
  (expand-file-name
   (read-directory-name "Repository dir: " name)))

(defun ma-github-ask-repo-info ()
  "Ask for repository’s name and local path."
  (let ((name (read-string "Repository name: ")))
    ;; Create a list to return with repository's name and dir
    (list name (ma-github-local-ask-path name))))

(defun ma-github-local-add-remote (name dir)
  "Add a “Git remote” for repository named NAME inside directory DIR."
  (shell-command
   (concat "git -C " dir " remote add origin "
           "git@github.com:"
           (getenv ma-github-env-user) "/"
           name ".git")))

(defun ma-github-local-create (name dir)
  "Create a new local repository named NAME inside directory DIR."
  (interactive (ma-github-ask-repo-info))
  (ignore-errors (make-directory dir t))
  (shell-command (concat "git -C " dir " init .")))

(defun ma-github-local-kickstart (dir)
  "Create a blank file inside DIR, “Git-add” it, then run “Git-commit”."
  (interactive "DRepository dir: ")
  (shell-command (concat "touch " dir "/README"))
  (shell-command (concat "git -C " dir " add " dir "/README"))
  (shell-command (concat "git -C " dir " commit -m 'Initial commit'")))

(defun ma-github-local-push (dir)
  "Run “Git-push” on the repository inside directory DIR."
  (shell-command
   (concat "git -C " dir " push -u origin master")))

(defun ma-github-create (name dir token kickstart git-push private)
  "Create repository named NAME on Github, and inside directory DIR.

Use the access token TOKEN to connect to the Github API.  If KICKSTART
is non-nil, create a blank file inside directory DIR, “Git-add” it,
then run “Git-commit”.  If GIT-PUSH is non-nil, run “Git-push” inside
DIR.  If PRIVATE is non-nil, the repository will be created “public,”
otherwise it will be created “private”."
  (interactive
   (nconc (ma-github-ask-repo-info)
          (list
           (getenv ma-github-env-token)
           (yes-or-no-p "Initial commit? ")
           (yes-or-no-p "Push to Github? ")
           (not (yes-or-no-p "Public? ")))))
  (setq ma-github-last-repo-name name)
  (setq ma-github-last-repo-dir dir)
  (let ((progress-reporter
         (make-progress-reporter "Creating repository..." 0 5)))
    (progress-reporter-update progress-reporter 0)

    (ma-github-local-create name dir)
    (progress-reporter-update progress-reporter 1)

    (ma-github-local-add-remote name dir)
    (progress-reporter-update progress-reporter 2)

    (ma-github-github-create name token private)
    (progress-reporter-update progress-reporter 3)

    (when kickstart (ma-github-local-kickstart dir))
    (progress-reporter-update progress-reporter 4)

    (when git-push (ma-github-local-push dir))
    (progress-reporter-done progress-reporter))
  (run-hooks 'ma-github-create-finalize-hook))
