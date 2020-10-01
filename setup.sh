function isLinux {
    uname | grep "Linux" > /dev/null
    return $?
}

function isMacos {
    uname | grep "Darwin" > /dev/null
    return $?
}

function installPython {
    local version="$1"

    if isLinux; then
        sudo apt-get install -y build-essential libbz2-dev libdb5.3-dev libexpat1-dev libffi-dev libgdbm-dev liblzma-dev libncurses5-dev libncursesw5-dev libreadline-dev libsqlite3-dev libssl-dev nmap zlib1g-dev
    elif isMacos; then
        brew install autoconf bzip2 gdbm libffi ncurses openssl@1.1 pkg-config readline sqlite unzip xz zlib
    fi

    if [ ! -d $HOME/python/$version ]; then
        wget --no-clobber https://www.python.org/ftp/python/$version/Python-$version.tar.xz
        tar xf Python-$version.tar.xz
        cd Python-$version

        if isLinux; then
            ./configure --prefix=$HOME/python/$version
        elif isMacos; then
            ./configure --prefix=$HOME/python/$version --with-openssl=$(brew --prefix openssl)
        fi

        make -j 3
        make install -j 3
        cd ..

        rm Python-$version.tar.xz
        rm -r Python-$version/

        $HOME/python/$version/bin/python3 -m pip install --upgrade pip
        $HOME/python/$version/bin/pip3 install virtualfish autopep8 isort
    fi
}

function generateSSHKey {
    local keyName="$1"
    local comment="$USER@$HOSTNAME $(date +'%Y-%m-%d')"

    if [ ! -f $HOME/.ssh/$keyName ]; then
        ssh-keygen -b 4096 -t rsa -q -f $HOME/.ssh/$keyName -P "" -C "$comment"
    fi
}

function setFishShellAsDefault {
    local fishShell=$(which fish)

    if [ ! $(echo $SHELL | grep fish) ]; then
        sudo chsh -s $fishShell $USER
    fi
}

function updateSystem {
    if isLinux; then
        sudo apt-get update
        sudo apt-get upgrade -y
        sudo apt-get install -y htop emacs git fish screen wget
        sudo apt-get autoremove
        sudo apt-get autoclean
    elif isMacos; then
        if ! $(which brew > /dev/null); then
            /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
        fi
        brew update
        brew upgrade
        brew install bash fish git htop wget
        brew cleanup
    fi
}

function main {
    updateSystem
    setFishShellAsDefault
    generateSSHKey 'key'
    installPython '3.8.6'
}

main


mkdir -p ~/./.config/fish/functions
cat << 'EOF' > ~/./.config/fish/functions/fish_prompt.fish
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

function fish_prompt
        set last_status $status

        set_color normal
        printf '['
        printf '%s ' (date "+%H:%M:%S")
        set_color purple
        printf '%s@%s ' (whoami) (hostname|cut -d . -f 1)
        set_color cyan
        printf '%s' (pwd | sed -e "s|^$HOME|~|")
        set_color normal
        printf ']'

        printf '%s' (__fish_git_prompt)
        set_color normal
        if [ $last_status = 0 ]
                set_color green
        else
                set_color red
        end

        printf '\n'
        if test $VIRTUAL_ENV
                printf "(%s) " (basename $VIRTUAL_ENV)
        end

        if [ $last_status = 0 ]
                printf 'λ '
        else
                printf 'ϵ '
        end
        set_color normal
end
EOF


mkdir -p ~/./.emacs.d
cat << 'EOF' > ~/./.emacs.d/init.el
;; MacOs specific settings
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil))


;; Don't create backup files
(setq make-backup-files nil)


;; Remove trailing whitespaces
(add-hook 'write-file-hooks 'delete-trailing-whitespace)


(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(defvar prelude-packages
  '(
    fish-mode
    py-autopep8
    py-isort
    ))
(defun prelude-packages-installed-p ()
  (cl-loop for p in prelude-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))
(unless (prelude-packages-installed-p)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;; autopep8
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=80"))


;; isort
(add-hook 'before-save-hook 'py-isort-before-save)
(setq py-isort-options '("--lines=80"))


(load-theme 'dichromacy t)
EOF
