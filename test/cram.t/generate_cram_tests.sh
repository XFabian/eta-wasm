#!/bin/bash
output_file="run.t"
echo "# Auto-generated cram tests" > "$output_file"
echo >> "$output_file"
for file in *.eta; do
  base="${file%.eta}"

  {
    echo "  \$ wetac --codegen $file;"
    echo "  > wat2wasm ${base}.wat -o ${base}.wasm;"
    echo "  > wasmer run ${base}.wasm --invoke main -- 0"
    echo
  } >> "$output_file"
done