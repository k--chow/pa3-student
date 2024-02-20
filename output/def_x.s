  section .text
  extern error
  global our_code_starts_here
our_code_starts_here:
  mov [rsp - 8], rdi

  mov rax, 11
  mov [rsp + -16], rax
  mov rax, [rsp + -16]
  ret
internal_error_non_bool:
  mov rsi, rax
  mov rdi, 99
  call error
internal_error_non_num:
  mov rsi, rax
  mov rdi, 299
  call error
error_overflow:
  mov rsi, rax
  mov rdi, 399
  call error
