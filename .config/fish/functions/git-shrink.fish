function git-shrink
        eval (which du) -s
        git gc --aggressive
        git repack -ad
        git prune
        eval (which du) -s
end
