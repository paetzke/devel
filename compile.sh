#!/bin/bash -eu

main() {
    local filename

    cat prototype.sh > setup.sh
    for filename in $(cd HOME; find . -type f | sort); do
        echo >> setup.sh
        echo >> setup.sh
        echo mkdir -p '~/'$(dirname $filename) >> setup.sh
        echo "cat << 'EOF' > ~/$filename" >> setup.sh
        cat HOME/$filename >> setup.sh
        echo "EOF" >> setup.sh
    done
}

main
