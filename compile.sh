#!/bin/bash -eu

main() {
    local filename

    cat setup.sh > setup
    for filename in $(cd HOME; find . -type f | sort); do
        echo >> setup
        echo >> setup
        echo mkdir -p '~/'$(dirname $filename) >> setup
        echo "cat << 'EOF' > ~/$filename" >> setup
        cat HOME/$filename >> setup
        echo "EOF" >> setup
    done

    echo chmod +x '~/bin/*' >> setup
    chmod +x setup
}


main
