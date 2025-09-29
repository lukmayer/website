#!/usr/bin/env bash

git add .

CHANGED_FILES=$(git status --porcelain --untracked-files=no | awk '{print $2}' | grep -E '\.qmd$')

if [ -z "$CHANGED_FILES" ]; then
    echo "No changed Quarto files to render"
    exit 0
fi

echo "Rendering changed files:"
echo "$CHANGED_FILES"

quarto render $CHANGED_FILES

echo "Enter a commit message:"
read commit_message

# --- Push source files to the source repository ---

git commit -m "$commit_message"
git remote set-url  origin git@github.com:lukmayer/website.git
git push -f origin master 


# --- Push rendered files to the GitHub Pages repository ---

cd _site

if [ ! -d ".git" ]; then
    git init
    git remote add origin git@github.com:lukmayer/lukmayer.github.io.git
else
    git remote set-url origin git@github.com:lukmayer/lukmayer.github.io.git
fi

git add .
git commit -m "$commit_message"
git push -f origin master:gh-pages  

cd ..
