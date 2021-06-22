showCompiledIr="--set=show_compiled_ir=true"
showCompiledIr=""
if make; then
	for linksFile in $(find ./tests/wasm -name "[bt]*.links" -print); do
		filenameNoExt=${linksFile:0:${#linksFile}-6};
		./links -d $showCompiledIr $linksFile --wat-output "$filenameNoExt.wat" && wat2wasm --enable-tail-call --relocatable --debug-names "$filenameNoExt.wat" -o "$filenameNoExt.wasm";
		# if $@; then wasm2wat "$filenameNoExt.wasm" -o "$filenameNoExt-decompile.wat"; fi
	done
fi
if $@; then
	cd "./tests/wasm/js-proj";
	node --experimental-wasm-return-call --experimental-wasm-reftypes src/index.js;
fi