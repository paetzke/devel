function git-shrink
        eval (which du) -s
        git gc --aggressive
        eval (which du) -s
end
