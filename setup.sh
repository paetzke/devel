#!/bin/bash -eu

main() {
    sudo apt-get update
    sudo apt-get install \
         build-essential \
         emacs \
         fish \
         git \
         htop \
         libbz2-dev \
         libreadline-dev \
         libsqlite3-dev \
         libssl-dev \
         make \
         zlib1g-dev

    # Install Python
    if [ ! -e ~/.pyenv ]; then
        git clone https://github.com/yyuu/pyenv.git ~/.pyenv
    fi
    ~/.pyenv/bin/pyenv install 3.5.1 --skip-existing

    # Configure fish shell
    mkdir -p ~/.config/fish/functions/
    cp .config/fish/functions/*.fish ~/.config/fish/functions/

    # Configure emacs
    mkdir -p ~/.emacs.d/
    cp -r .emacs.d/* ~/.emacs.d/
    if [ ! -f ~/.emacs.d/my-config/my-local.el ]; then
        cp ~/.emacs.d/my-config/my-local.l ~/.emacs.d/my-config/my-local.el
    fi
}

main
