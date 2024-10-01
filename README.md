# CRVJIT

CRVJIT is a Cranelift based RISC-V JIT emulator.

It implements the RV64 I, M, A, Zicsr ISA extensions. Zifencei and privileged extensions **WIP**.

## Features

- [x] RV64I
- [x] RV64M
- [x] RV64A
- [x] RV64Zicsr
- [ ] RV64Zifencei
- [ ] RV64 Privileged
- [ ] UART
- [ ] CLINT
- [ ] PLIC

## Usage

```sh
cargo run --release --bin crvjit -- <path-to-binary>
```

You may want to turn on logging to see what's going on:

```sh
RUST_LOG=debug,cranelift=off cargo run --release --bin crvjit -- <path-to-binary>
```

## Running riscv-tests

To run the [riscv-tests](https://github.com/riscv-software-src/riscv-tests) suite, you need to convert the ELF binaries to raw binaries first:

``` sh
riscv64-unknown-elf-objcopy -O binary <path-to-binary> <path-to-binary>.bin
```

