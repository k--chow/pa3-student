  section .text
  extern error
  global our_code_starts_here
our_code_starts_here:
  mov [rsp - 8], rdi

  mov rax, 0
  and rax, 1
  cmp rax, 0
  jne near not_bool
  mov rax, 0x2
  jmp near end_not_bool
not_bool:
  mov rax, 0
end_not_bool:
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
