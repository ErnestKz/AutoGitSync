# AutoGitSync
Quick and dirty program that runs in the background and listens to file changes and pushes them to specified repos and branches and also periodically pulls changes from them.

Program reads the config file whose path is specified in the source code. 

The config file expected is in the format of `<repo_path>,<branch>` each on a separate line. 
