showCompiledIr="--set=show_compiled_ir=true"
showCompiledIr=""
measurePerformance="--set=measure_performance=true"
wat2wasm="wat2wasm --enable-tail-call --relocatable --debug-names"
kill $(ps | grep '.*_build/default/bin/links.exe' | grep -v grep | awk '{print $1}');
if make; then
	for linksFile in $(find ./tests/wasm/performance-proj -name "tailcall.links" -print); do
		filenameNoExt=${linksFile:0:${#linksFile}-6};
		filenameNoPath=$(basename $filenameNoExt);
		echo "processing server file";
		./links $showCompiledIr --client-backend wasm --wasm-performance $linksFile --wat-output "$filenameNoExt.wat" && $wat2wasm "$filenameNoExt.wat" -o "$filenameNoExt.wasm";
		echo "processing client wasm file";
		showCompiledIr=""
		./links $showCompiledIr --client-backend wasm "$filenameNoExt-client-wasm.links" --wat-output "$filenameNoExt-client-wasm.wat" && $wat2wasm "$filenameNoExt-client-wasm.wat" -o "$filenameNoExt-client-wasm.wasm" && cd "./tests/wasm/performance-proj" && node --experimental-wasm-return-call --experimental-wasm-reftypes "$filenameNoPath-client-wasm--index.js" >> "./server_execution_time.txt" && cd ../../..;
		echo "processing client js file";
		./links $showCompiledIr "$filenameNoExt-client-js.links";
	done
fi