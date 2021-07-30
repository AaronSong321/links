showCompiledIr="--set=show_compiled_ir=true"
showCompiledIr=""
measurePerformance="--set=measure_performance=true"
source ~/.bash_profile
measurePerformance=""
kill $(ps | grep '.*_build/default/bin/links.exe' | grep -v grep | awk '{print $1}')
if make; then
	for linksFile in $(find ./tests/wasm -name "[t]est*.links" -print); do
		filenameNoExt=${linksFile:0:${#linksFile}-6}
		echo "compiling $filenameNoExt"
		./links -d $showCompiledIr --client-backend notwasm $measurePerformance $linksFile --wat-output "$filenameNoExt.wat" && jankscripten compile --stdlib /Users/aarons/Aaron/Master/Project/wasm/jankscripten/stdlib.notwasm "$filenameNoExt.notwasm"
		
	done
fi