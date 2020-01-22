# Capture packages (a, b, ...), not functions or other objects (f1, f2, ...)
# Do not capture packages in invalid calls (x1, x2, ...)

# valid uses of import::from
import::from(a, f1, f2, f3)
import::from(f1, .from = b)

# valid uses of import::here
import::here(f1, f2, f3, .from = c)
# invalid uses of import::here – should not infer a dependency
import::here(f1, x2)  # no .from argument
import::here(f1, f2, f3, x3)  # as above
import::here(f1) # no package is specified

# valid uses of import::into
import::into(f1, f2, f3, .into = "imports::pkg", .from = d)
import::into("imports::pkg", f1, f2, .from = e)
# invalid uses of import::into
import::into(f1, f2, x4, .into = "imports::pkg")  # no .from argument
import::into(f1, x5)  # no .from or .into
import::into(f1)  # no package specified at all
