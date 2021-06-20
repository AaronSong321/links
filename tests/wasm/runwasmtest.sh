# make && ./links -d --set=show_compiled_ir=true ./tests/wasm/b.links && wat2wasm ./tests/wasm/a.wat -o ./tests/wasm/a.wasm
showCompiledIr="--set=show_compiled_ir=true"
# showCompiledIr=""
if make; then
	for linksFile in $(find ./tests/wasm -name "[ab]*.links" -print); do
		filenameNoExt=${linksFile:0:${#linksFile}-6};
		./links -d $showCompiledIr $linksFile --wat-output "$filenameNoExt.wat" && wat2wasm "$filenameNoExt.wat" -o "$filenameNoExt.wasm" && wasm2wat "$filenameNoExt.wasm" -o "$filenameNoExt-decompile.wat";
	done
fi
