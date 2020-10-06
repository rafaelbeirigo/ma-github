Use Emacs to create Git repositories in github.com.

# Usage:

* Add `ma-github.el` to your Emacs load path

* Create a personal access token from Github at https://github.com/settings/tokens/new

* Set the environment variable MA_GITHUB_TOKEN with your new token

* Set the environment variable MA_GITHUB_USER with your Github user

Then run `M-x ma-github-create-repo` in Emacs, which will:

* create the repository in github.com

* create a folder for the local repository, and `git init` it

* create a blank README file inside this folder and `git add` it

* do the first commit, with the message 'Initial commit'

* `git push -u origin master` it to the newly created github repo
