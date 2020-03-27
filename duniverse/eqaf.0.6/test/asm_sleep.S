global eqaf_sleep

section .data
  timespec:
    tv_sec  dq 1
    tv_nsec dq 0

section .text
  eqaf_sleep:
    mov rax, 35
    mov rdi, timespec
    xor rsi, rsi
    syscall
    mov rax, 0
    ret
