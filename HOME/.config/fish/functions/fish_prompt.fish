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
