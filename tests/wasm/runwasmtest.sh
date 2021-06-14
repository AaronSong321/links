# make && ./links -d --set=show_compiled_ir=true ./tests/wasm/b.links && wat2wasm ./tests/wasm/a.wat -o ./tests/wasm/a.wasm
if make; then
	for linksFile in $(find ./tests/wasm -name "[ab]*.links" -print); do
		filenameNoExt=${linksFile:0:${#linksFile}-6};
		./links -d --set=show_compiled_ir=true $linksFile --set=debug=true --wat-output "$filenameNoExt.wat" && wat2wasm "$filenameNoExt.wat" -o "$filenameNoExt.wasm" && wasm2wat "$filenameNoExt.wasm" -o "$filenameNoExt-decompile.wat";
	done
fi
