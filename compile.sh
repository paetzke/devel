#!/bin/bash -eu

main() {
    local filename

    cat prototype.sh > install.sh
    for filename in $(cd HOME; find . -type f | sort); do
        if [ "$filename" = "./.DS_Store" ]; then
            continue
        fi

        echo >> install.sh
        echo >> install.sh
        echo mkdir -p '~/'$(dirname $filename) >> install.sh
        echo "cat << 'EOF' > ~/$filename" >> install.sh
        cat HOME/$filename >> install.sh
        echo "EOF" >> install.sh
    done

    echo chmod +x '~/bin/*' >> install.sh
    chmod +x install.sh
}

main
