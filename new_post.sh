#!/bin/bash

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

# Create the .qmd file and insert the YAML header
# Using printf for potentially better compatibility than heredoc in some shells
printf -- "---\n" > "$FULL_FILENAME"
printf "title: \"\"\n" >> "$FULL_FILENAME"
printf "description: \"\"\n" >> "$FULL_FILENAME"
printf "author: \"Luke\"\n" >> "$FULL_FILENAME"
printf "date: \"%s\"\n" "$CURRENT_DATE" >> "$FULL_FILENAME"
printf "#categories:\n" >> "$FULL_FILENAME"
printf "# - quarto\n" >> "$FULL_FILENAME"
printf "\n" >> "$FULL_FILENAME"
printf "comments:\n" >> "$FULL_FILENAME"
printf "  giscus:\n" >> "$FULL_FILENAME"
printf "    repo: lukmayer/site_comments\n" >> "$FULL_FILENAME"
printf "    theme: dark\n" >> "$FULL_FILENAME"
printf "    mapping: url\n" >> "$FULL_FILENAME"
printf "    reactions-enabled: true\n" >> "$FULL_FILENAME"
printf "---\n" >> "$FULL_FILENAME"

echo "File '$FULL_FILENAME' created successfully in '$PWD'."

exit 0
