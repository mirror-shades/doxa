.text
.balign 8
.globl calloc_2_2_5
.type calloc_2_2_5, %function
.symver calloc_2_2_5, calloc@@GLIBC_2.2.5, remove
calloc_2_2_5: .quad 0
.balign 8
.globl __libc_memalign_2_2_5
.type __libc_memalign_2_2_5, %function
.symver __libc_memalign_2_2_5, __libc_memalign@@GLIBC_2.2.5, remove
__libc_memalign_2_2_5: .quad 0
.balign 8
.globl malloc_2_2_5
.type malloc_2_2_5, %function
.symver malloc_2_2_5, malloc@@GLIBC_2.2.5, remove
malloc_2_2_5: .quad 0
.balign 8
.globl free_2_2_5
.type free_2_2_5, %function
.symver free_2_2_5, free@@GLIBC_2.2.5, remove
free_2_2_5: .quad 0
.balign 8
.globl _dl_mcount_2_2_5
.type _dl_mcount_2_2_5, %function
.symver _dl_mcount_2_2_5, _dl_mcount@@GLIBC_2.2.5, remove
_dl_mcount_2_2_5: .quad 0
.balign 8
.globl realloc_2_2_5
.type realloc_2_2_5, %function
.symver realloc_2_2_5, realloc@@GLIBC_2.2.5, remove
realloc_2_2_5: .quad 0
.balign 8
.globl __tls_get_addr_2_3
.type __tls_get_addr_2_3, %function
.symver __tls_get_addr_2_3, __tls_get_addr@@GLIBC_2.3, remove
__tls_get_addr_2_3: .quad 0
.rodata
.data
.balign 8
.globl __libc_stack_end_2_2_5
.type __libc_stack_end_2_2_5, %object
.size __libc_stack_end_2_2_5, 8
.symver __libc_stack_end_2_2_5, __libc_stack_end@@GLIBC_2.2.5, remove
__libc_stack_end_2_2_5: .fill 8, 1, 0
.balign 8
.globl _r_debug_2_2_5
.type _r_debug_2_2_5, %object
.size _r_debug_2_2_5, 40
.symver _r_debug_2_2_5, _r_debug@@GLIBC_2.2.5, remove
_r_debug_2_2_5: .fill 40, 1, 0
