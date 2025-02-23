#!/bin/bash

git add .

# Get list of changed .qmd or .md files in git working tree
CHANGED_FILES=$(git status --porcelain --untracked-files=no | awk '{print $2}' | grep -E '\.qmd$')

if [ -z "$CHANGED_FILES" ]; then
    echo "No changed Quarto files to render"
    exit 0
fi

echo "Rendering changed files:"
echo "$CHANGED_FILES"

# Render changed files
quarto render $CHANGED_FILES

# Prompt for a commit message
echo "Enter a commit message:"
read commit_message

# --- Push source files to the main repository (lukmayer/website.git) ---

# Ensure the current directory is your main project directory (not _site)
# Add and commit changes to the source repo

git commit -m "$commit_message"

git remote set-url  origin git@github.com:lukmayer/website.git

# Push changes to the main repository (lukmayer/website.git)
# Ensure 'origin' points to the source repo (lukmayer/website.git)
git push -f origin master 


# --- Push rendered files to the GitHub Pages repository (lukmayer.github.io.git) ---

# Navigate to the _site directory (contains the rendered files)
cd _site

# Initialize a new Git repository inside _site if it doesn't exist already
if [ ! -d ".git" ]; then
    git init
    git remote add origin git@github.com:lukmayer/lukmayer.github.io.git
else
    git remote set-url origin git@github.com:lukmayer/lukmayer.github.io.git
fi

# Add and commit the rendered site files
git add .
git commit -m "$commit_message"

# Force push the rendered files to the gh-pages branch of the GitHub Pages repo
git push -f origin master:gh-pages  # Ensure that 'gh-pages' is the correct branch for GitHub Pages

# Navigate back to the root directory of the project
cd ..
