#!/bin/bash

# Build the site with Quarto
quarto render

# Navigate to the _site directory
cd _site

# Initialize a new Git repository
git init
git add .
git commit -m "Deploy updates"

# Force push to the gh-pages branch of the GitHub Pages repository
git remote add origin git@github.com:lukmayer/lukmayer.github.io.git
git push -f origin master:gh-pages

# Clean up
cd ..
rm -rf _site/.git
