  section .text
  extern error
  global our_code_starts_here
our_code_starts_here:
  mov [rsp - 8], rdi

  mov rax, 11
  mov [rsp + -16], rax
  mov rax, [rsp + -16]
  mov [rsp + -32], rax
  mov rax, 15
  mov [rsp + -40], rax
  mov rax, [rsp + -32]
  cmp rax, [rsp + -40]
  je near equal_true
  mov rax, 0
  jmp near end_equal
equal_true:
  mov rax, 0x2
end_equal:
  cmp rax, 0x2
  je near temp_if_branch_1
  mov rax, 17
  jmp near temp_end_if_2
temp_if_branch_1:
  mov rax, 15
temp_end_if_2:
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
