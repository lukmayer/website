#!/usr/bin/env bash

# Define the target directory relative to the script's execution location
TARGET_DIR="blog/posts"

# Check if the target directory exists
if [ ! -d "$TARGET_DIR" ]; then
  echo "Error: Directory '$TARGET_DIR' not found."
  echo "Please ensure the directory exists or create it."
  exit 1
fi

# Navigate to the target directory
cd "$TARGET_DIR" || exit 1

# Ask the user for the filename (without extension)
read -p "Enter the filename (without extension): " FILENAME

# Check if the filename is provided
if [ -z "$FILENAME" ]; then
  echo "Error: Filename cannot be empty."
  exit 1
fi

# Construct the full filename
FULL_FILENAME="${FILENAME}.qmd"

# Get the current date in ISO 8601 format (YYYY-MM-DD)
CURRENT_DATE=$(date +%Y-%m-%d)

# Create the .qmd file and insert the YAML header using a heredoc
cat << EOF > "$FULL_FILENAME"
---
title: ""
description: ""
author: "Luke"
# image:
date: "$CURRENT_DATE"
#categories:
# - quarto

format:
  html:
    include-after-body: ../../_includes/utterances.html

draft: true
---
EOF

# Optional: Check if the heredoc write was successful
if [ $? -ne 0 ]; then
    echo "Error: Failed to write to '$FULL_FILENAME'."
    exit 1
fi

echo "File '$FULL_FILENAME' created successfully in '$PWD'."

exit 0
