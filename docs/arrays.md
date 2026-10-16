# Arrays

Arrays are dynamic by default. They are declared by adding square brackets after the type. Nested arrays are declared by using multiple brackets. By default arrays are stored on the heap.


```
var arr :: int [] is [1, 2, 3, 4]
var n_arr :: int[][] is [[1, 2], [3, 4]]

@print("{arr[1]}") # 2
@print("{n_arr[0][1]}") # 2
```

Arrays can be given fixed sizes within type declarations. When given a fixed size, arrays are stored on the stack.


```
var chessboard :: string[8][8] # 8x8 string matrix on stack
var buffer :: int[256] # 256 integers on stack
```

If an array is made constant it is stored statically and refrenced by pointer.

```
const medals is ["gold", "silver", "bronze"]
```

Arrays are handled by intrinsic methods such as @push, @pop, @slice, etc. An index out of bounds, read or written, and a `@pop` of an empty array are runtime faults ([Runtime faults](methods.md#runtime-faults)); the standard library's `methods` sublibrary has versions that check first.

Arrays will be automatically cast into the requested array storage if possible.

```
var dyn_arr is [1, 2, 3] # heap allocated
var fixed_arr :: int[3] is dyn_arr # copy is made to the stack
const static_arr is fixed_arr # static allocation made and refrenced
```

### Performance

Dynamic arrays are allocated from the current scope's arena. Array elements are stored in a single contiguous allocation, and mutations that require dynamic storage conversion copy the element data in bulk. Assigning one array variable to another copies it: the two never share elements (see [memory.md](memory.md)). A fresh array (a literal, a call's result) is moved, not copied. To change an array somewhere else, lend it with `^`: a `^` parameter, or `each n in ^arr` to change its elements in a loop.