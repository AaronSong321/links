measurePerformance="--set=measure_performance=true"
wat2wasm="wat2wasm --enable-tail-call --relocatable --debug-names"
kill $(ps | grep '.*_build/default/bin/links.exe' | grep -v grep | awk '{print $1}')
files=( "./tests/wasm/performance-proj/tailcall.links" )
files=("./tests/wasm/performance-proj/mandelbrot.links")
files=("./tests/wasm/performance-proj/pi.links")
if make; then
	for linksFile in $files; do
		filenameNoExt=${linksFile:0:${#linksFile}-6}
		filenameNoPath=$(basename $filenameNoExt)
		./links $showCompiledIr --client-backend wasm --wasm-performance $linksFile --wat-output "$filenameNoExt.wat" 
		cd "./tests/wasm/performance-proj"
		node --experimental-wasm-return-call --experimental-wasm-reftypes "$filenameNoPath-client-wasm-perf--index.js"
		cd ../../..
		echo "processing client js file"
		./links "$filenameNoExt-client-js.links"
	done
fi