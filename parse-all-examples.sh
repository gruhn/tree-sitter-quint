# Make sure the examples directory exists:
if [ ! -d "examples" ]; then
  echo "examples directory does not exist."
  exit 1
fi

# Stop if one of the parse commands returns with 
# non-zero exit code:
set -e

# Run parser on every example:
for file in examples/*.qnt; do
  if [ -f "$file" ]; then
    # Will return exit code 1, if parsing fails:
    tree-sitter parse --quiet "$file"
  fi
done
