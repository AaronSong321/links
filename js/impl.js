function printInteger(a) {
	console.log(a)
}
function printFloat(a) {
	console.log(a)
}
function _currentTimeMilliseconds() {
	return performance.now()
}
const currentTimeMilliseconds = LINKS.kify(_currentTimeMilliseconds)
function _standardDeviation(arr) {
	const mean = arr.reduce((acc, val) => acc+val, 0)/arr.length;
	return [mean, Math.sqrt(arr.reduce((acc,val) => acc.concat((val-mean)**2),[])
		.reduce((acc,val)=>acc+val,0)/(arr.length))]
}
function _performanceFunc(f, times) {
	if (times == undefined)
		times=20
	const t = []
	for (var i=0; i<times; ++i) {
		const before = performance.now()
		f()
		const after = performance.now()
		t.push(after-before)
	}
	const std = _standardDeviation(t)
	const l = "mean="+std[0]+",std deviation="+std[1]
	console.log(l)
}
function performanceFunc(f, times, k) {
	_performanceFunc(f, times)
	return _applyCont(k, {})
}
function changeInnerHtml_hidden(label, contents, k) {
	document.getElementById(label).innerText = contents
	return _applyCont(k, {})
}
