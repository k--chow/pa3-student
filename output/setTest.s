  section .text
  extern error
  global our_code_starts_here
our_code_starts_here:
  mov [rsp - 8], rdi

  mov rax, 3
  mov [rsp + -16], rax
  mov rax, 5
  mov [rsp + -16], rax
  mov rax, [rsp + -16]
  ret
