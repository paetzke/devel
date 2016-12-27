#!/bin/bash -u

main() {
    local filename

    for filename in $(cd HOME; find . -type f | sort); do
        if [ ! -f ~/$filename ]; then
            continue
        fi

        if ! diff HOME/$filename ~/$filename; then
            echo $filename
        fi
    done
}


main
