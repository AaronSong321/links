"use strict"
function ToArrayBuffer(buffer) {
    var a = new ArrayBuffer(buffer.length)
    var v = new Uint8Array(a)
    for (var i=0; i<buffer.length; ++i) {
        v[i] = buffer[i]
    }
    return a
}
const fs = require("fs")
async function InstantiateWasmFile(filename, importObject) {
    var data = fs.readFileSync(filename)
    var _wasmArrayBuffer = ToArrayBuffer(data)
    const {_, instance} = await WebAssembly.instantiate(_wasmArrayBuffer, importObject)
    return instance
}

InstantiateWasmFile("../tailcall.wasm", {imports: {

} }).then(module => {
    const factorial = module.exports.factorial_2236
    const count = module.exports.count_2239
    // factorial(BigInt(100000))
    const before = performance.now()
    console.log(module.exports.count2_2244(BigInt(10000000000), BigInt(0)))
    const time = performance.now() - before
    console.log(time)
    // console.log(1)
})

// InstantiateWasmFile("../notailcall.wasm", {imports: {

// } }).then(module => {
//     const factorial = module.exports.factorial_2236
//     const count = module.exports.count_2239
//     count(BigInt(100000000))
//     console.log(1)
// })