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


mkdir -p ~/./.config/fish
cat << 'EOF' > ~/./.config/fish/config.fish
eval (python3 -m virtualfish)
set -gx VIRTUALFISH_HOME ~/python/ve
set -gx PATH $HOME/bin $PATH
EOF


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

        if test -f .keep-until
                printf " "
                set_color -b red
                printf "%s" (cat .keep-until)
                set_color normal
        end

        printf '%s' (__fish_git_prompt)
        set_color normal
        if [ $last_status = 0 ]
                set_color green
        else
                set_color red
        end

        if test -f .pyvenv
                if not test $VIRTUAL_ENV
                        vf activate (cat .pyvenv)
                end
        else
                if test $VIRTUAL_ENV
                        vf deactivate
                end
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


mkdir -p ~/./.config/fish/functions
cat << 'EOF' > ~/./.config/fish/functions/fish_title.fish
function fish_title
        true
end
EOF


mkdir -p ~/./.config/fish/functions
cat << 'EOF' > ~/./.config/fish/functions/git-shrink.fish
function git-shrink
        eval (which du) -s
        git gc --aggressive
        git repack -ad
        git prune
        eval (which du) -s
end
EOF


mkdir -p ~/./.config/fish/functions
cat << 'EOF' > ~/./.config/fish/functions/l.fish
function l
    ls -halt $argv
end
EOF


mkdir -p ~/./.config/fish/functions
cat << 'EOF' > ~/./.config/fish/functions/ll.fish
function ll
        ls -lah $argv
end
EOF


mkdir -p ~/./.config
cat << 'EOF' > ~/./.config/keepuntil
readonly KEEPUNTIL_GRAVEYARD=~/Dropbox/eternity/
readonly KEEPUNTIL_ROOT=~/
EOF


mkdir -p ~/./.emacs.d
cat << 'EOF' > ~/./.emacs.d/init.el

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(setq server-use-tcp t)



(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil))

(toggle-scroll-bar -1)
;; no backup files
(setq make-backup-files nil)
;; remove whites
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; don't autosave
(setq auto-save-default nil)
;; no tabs
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(tool-bar-mode -1)

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)

; Some initial langauges we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
                                        ;(sh . t)
   (python . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (calc . t)))


(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar prelude-packages
  '(
    ;atom-dark-theme
    fish-mode
    ;format-sql
                                        ;graphviz-dot-mode
    json-reformat
    markdown-mode
    py-autopep8
    py-isort
;    py-yapf
    yaml-mode
    ))

(defun prelude-packages-installed-p ()
  (cl-loop for p in prelude-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
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


mkdir -p ~/./bin
cat << 'EOF' > ~/./bin/keepuntil
#!/bin/bash -eu


source_config() {
    source ~/.config/keepuntil

    test -d "$KEEPUNTIL_GRAVEYARD"
    test -d "$KEEPUNTIL_ROOT"
}


die() {
    local msg="$1"

    echo $msg
    exit 1
}


get_absolute_path() {
    local path="$1"

    pushd $(dirname $path) > /dev/null
    echo $(pwd)
    popd > /dev/null
}


compress_folder() {
    local path="$1"
    local date="$2"
    local compress_name

    compress_name=${path/\//}
    compress_name=${compress_name//\//\--}
    compress_name="$KEEPUNTIL_GRAVEYARD$date-$compress_name.tar.gz"

    if [ -f "$compress_name" ]; then
        die "$compress_name already exists."
    fi

    pushd $path > /dev/null
    echo $compress_name
    tar czvf $compress_name * > /dev/null
    chmod 444 $compress_name
    popd > /dev/null
    rm -rf $path
}


main() {
    local filename
    local content
    local now=$(date +%s)
    local keepuntil

    source_config

    for filename in $(find $KEEPUNTIL_ROOT -name .keepuntil 2> /dev/null); do
        echo "FOUND $(dirname $filename)"

        content=$(cat $filename)
        keepuntil=$(date -j -f "%Y-%m-%d" $content "+%s")
        echo -e "\tvalid until $content"

        if [[ $now > $keepuntil ]]; then
            echo -e "\tMOVING"
            compress_folder $(get_absolute_path $filename) "$content"
        fi
    done
}


main
EOF
chmod +x ~/bin/*
