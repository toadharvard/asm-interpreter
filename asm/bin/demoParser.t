  $ ./demoParser.exe <<- EOF
  > section .text
  > fibonachch:
  > push rbp
  > mov rbx, 0x2A
  > mov rax, 0x0
  > mov rcx, 1
  > cmp rbx, 1
  > je fibonachchEnd
  > cmp rbx, 2
  > je fibonachchTwo
  > sub rbx, 1
  > fibonachchStart:
  > sub rbx, 1
  > xor rax, rcx
  > xor rcx, rax
  > xor rax, rcx
  > add rax, rcx
  > cmp rbx, 0
  > je fibonachchEnd
  > jmp fibonachchStart
  > fibonachchTwo:
  > mov rax, 1
  > fibonachchEnd:
  > pop rbp
  > ret
  > EOF
  (Directive (Section .text))
  (LabelDecl fibonachch)
  (Instruction (Push (Reg_64 Rbp)))
  (Instruction (Mov (Reg_64_Imm_a (Rbx, (ImmInt 42)))))
  (Instruction (Mov (Reg_64_Imm_a (Rax, (ImmInt 0)))))
  (Instruction (Mov (Reg_64_Imm_a (Rcx, (ImmInt 1)))))
  (Instruction (Cmp (Reg_64_Imm_a (Rbx, (ImmInt 1)))))
  (Instruction (Je (Label (LabelRef fibonachchEnd))))
  (Instruction (Cmp (Reg_64_Imm_a (Rbx, (ImmInt 2)))))
  (Instruction (Je (Label (LabelRef fibonachchTwo))))
  (Instruction (Sub (Reg_64_Imm_a (Rbx, (ImmInt 1)))))
  (LabelDecl fibonachchStart)
  (Instruction (Sub (Reg_64_Imm_a (Rbx, (ImmInt 1)))))
  (Instruction (Xor (Reg_64_Reg_64 (Rax, Rcx))))
  (Instruction (Xor (Reg_64_Reg_64 (Rcx, Rax))))
  (Instruction (Xor (Reg_64_Reg_64 (Rax, Rcx))))
  (Instruction (Add (Reg_64_Reg_64 (Rax, Rcx))))
  (Instruction (Cmp (Reg_64_Imm_a (Rbx, (ImmInt 0)))))
  (Instruction (Je (Label (LabelRef fibonachchEnd))))
  (Instruction (Jmp (Label (LabelRef fibonachchStart))))
  (LabelDecl fibonachchTwo)
  (Instruction (Mov (Reg_64_Imm_a (Rax, (ImmInt 1)))))
  (LabelDecl fibonachchEnd)
  (Instruction (Pop (Reg_64 Rbp)))
  (Instruction (Ret (Nothing)))
