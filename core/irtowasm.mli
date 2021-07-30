val run : Backend.result -> string -> string -> unit

module type WASM_PERFORMANCE = sig
	(* val javascript_time: int ref *)
	val measure_wasm_performance: bool Settings.setting
	val trans: string -> unit
end
module Wasm_performance: WASM_PERFORMANCE