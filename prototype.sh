#!/bin/bash -eu

if uname | grep Darwin > /dev/null; then
    if ! which brew; then
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
        brew update
    fi

    brew install bash fish git htop wget python3
    pip3 install virtualfish autopep8 isort
fi

git config --global alias.co "checkout"
git config --global alias.in "!git init && git commit -m 'Initial commit' --allow-empty"
git config --global alias.df "diff --patience"
git config --global alias.lg "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
git config --global alias.mn "merge --no-ff"
git config --global core.editor "emacs"
