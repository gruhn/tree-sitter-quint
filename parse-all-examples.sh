# Make sure the examples directory exists:
if [ ! -d "examples" ]; then
  echo "examples directory does not exist."
  exit 1
fi

# Set exit code to 1 if parsing fails for any of the 
# example files:
exit_code=0

# Run parser on every example:
echo "Trying to parse all example Quint files: "
for file in examples/*.qnt; do
  if [ -f "$file" ]; then
    # Will return exit code 1, if parsing fails:
    tree-sitter parse --quiet "$file" > /dev/null

    if [ "$?" -eq "0" ]; then
      echo "success: $file"
    else
      echo "failed: $file"
      exit_code=1
    fi
  fi
done

exit $exit_code
