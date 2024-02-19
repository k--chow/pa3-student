  section .text
  extern error
  global our_code_starts_here
our_code_starts_here:
  mov [rsp - 8], rdi

  mov rax, 0x2
  cmp rax, 0x2
  je near temp_if_branch_1
  mov rax, 13
  jmp near temp_end_if_2
temp_if_branch_1:
  mov rax, 11
temp_end_if_2:
  ret
