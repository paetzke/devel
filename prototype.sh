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
