#!/bin/bash
output_file="run.t"
echo "# Auto-generated cram tests" > "$output_file"
echo >> "$output_file"
for file in *.eta; do
  base="${file%.eta}"

  {
    echo "  \$ wetac --parse $file;"
    echo
  } >> "$output_file"
done