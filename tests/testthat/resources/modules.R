module({
	import("A")
	import(B)
	import(from = "C")
	import(symbol, from = D)
})

# NOTE: these should be ignored as they are not
# called within a module block
import("e")
import(f)

# NOTE: fully scoped modules::import calls should
# be added to dependencies
modules::import("G")
modules::import(H)

