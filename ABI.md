ABI's should be declarable via the language.

For example, the standard x86_64 calling convention should be declarable via something like:
```
import(abi)

set(abiSystemVX86_64 abi.make(
    abi.return(abi.reg.eax abi.reg.edx)
    abi.arg(0 abi.reg.rdi)
    abi.arg(1 abi.reg.rsi)
    abi.arg(2 abi.reg.rdx)
    abi.arg(3 abi.reg.rcx)
    abi.arg(4 abi.reg.r8)
    abi.arg(5 abi.reg.r9)
    abi.stackAlignment(1)
    abi.scratchRegisters(
        abi.reg.rax
        abi.reg.rdi
        abi.reg.rsi
        abi.reg.rdx
        abi.reg.rcx
        abi.reg.r8
        abi.reg.r9
        abi.reg.r10
        abi.reg.r11
    )
    abi.preservedRegisters(
        abi.reg.ebx
        abi.reg.esi
        abi.reg.edi
        abi.reg.ebp
        abi.reg.esp
    )
))

```