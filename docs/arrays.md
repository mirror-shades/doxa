# Arrays

Arrays are dynamic by default. They are declared by adding square brackets after the type. Nested arrays are declared by using multiple brackets. By default arrays are stored on the heap.


```
var arr :: int [] is [1, 2, 3, 4]
var n_arr :: int[][] is [[1, 2], [3, 4]]

@print("{arr[1]}") // 2
@print("{n_arr[0][1]}") // 2
```

Arrays can be given fixed sizes within type declarations. When given a fixed size, arrays are stored on the stack.


```
var chessboard :: string[8][8] // 8x8 string matrix on stack
var buffer :: int[256] // 256 integers on stack
```

If an array is made constant it is stored statically and refrenced by pointer.

```
const medals is ["gold", "silver", "bronze"]
```

Arrays are handled by intrinsic methods such as @push, @pop, @slice, etc. These intrinsics are inherently unsafe so usage of the standard library is encouraged.

Arrays will be automatically cast into the requested array storage if possible.

```
var dyn_arr is [1, 2, 3] // heap allocated
var fixed_arr :: int[3] is dyn_arr // copy is made to the stack
const static_arr is fixed_arr // static allocation made and refrenced
```