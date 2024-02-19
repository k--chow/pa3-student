  section .text
  extern error
  global our_code_starts_here
our_code_starts_here:
  mov [rsp - 8], rdi

  mov rax, 0x2
  ret
