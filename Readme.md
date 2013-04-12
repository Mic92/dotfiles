# Install with homeshick

    $ curl -sL https://raw.github.com/andsens/homeshick/master/install.sh | bash:

    $ ~/.homeshick clone git://github.com/Mic92/dotfiles.git

    $ ~/.homeshick clone git://github.com/Mic92/scripts.git

    $ (cd ~/.homesick/repos/dotfiles && git submodule foreach --recursive "git pull -u origin master")

    $ ~/.homeshick symlink
