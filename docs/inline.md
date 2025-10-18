my idea for inline zig:

```
var counter is 1
var name is "Bob"
#zig { // this creates a new zig program which has access to the doxa namespace for atomic types (tetras reduce to their truthy values)
    const std = @import("std")
    
    std.debug.print( "{s}", .{name}}); // this pulls in the name variable as a copy
    counter = counter + 1; // this pulls in the counter variable as a copy
} #marshal counter // this marshals the copy into the given variable
s
it is compiled to .o and caches after HIR gen and before the pipeline splits. the object file can be linked in compilation or executed jit during vm