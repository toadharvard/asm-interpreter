  $ ./demoParser.exe <<- EOF
  > section .text
  > global _start
  > _start:
  >    ; Инициализируем регистры для хранения предыдущих двух чисел Фибоначчи
  >    mov rcx, 11 ; Ишем 10-ое число Фибоначчи 
  >    mov rax, 0 ; F(0)
  >    mov rbx, 1 ; F(1)
  >    
  >    cmp rcx, 1
  >    jle exit ; Если n <= 1, результат уже в rax
  >    sub rcx, 1 ; Первое число Фибоначчи уже в rax, начинаем с F(2)
  >    
  > fib_loop:
  >    add rax, rbx ; F(n) = F(n-1) + F(n-2)
  >    ; Обмен значениями rax и rbx, используя xor swap trick
  >    xor rax, rbx
  >    xor rbx, rax
  >    xor rax, rbx
  >    ; Теперь rax содержит F(n-1), rbx содержит F(n)
  >    
  >    sub rcx, 1
  >    cmp rcx, 1
  >    jne fib_loop
  >    
  > exit:
  >    ; Завершение программы и возврат значения в rax
  >    mov rdi, 0 ; Код возврата
  >    mov rax, 60 ; syscall номер для exit
  >    syscall
  > EOF
  (Directive (Section .text))
  (Directive (Global _start))
  (LabelDecl _start)
  (Instruction (Mov (Reg_64_imm_a (Rcx, (Imm_int 11)))))
  (Instruction (Mov (Reg_64_imm_a (Rax, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rbx, (Imm_int 1)))))
  (Instruction (Cmp (Reg_64_imm_a (Rcx, (Imm_int 1)))))
  (Instruction (Jle (Label exit)))
  (Instruction (Sub (Reg_64_imm_a (Rcx, (Imm_int 1)))))
  (LabelDecl fib_loop)
  (Instruction (Add (Reg_64_reg_64 (Rax, Rbx))))
  (Instruction (Xor (Reg_64_reg_64 (Rax, Rbx))))
  (Instruction (Xor (Reg_64_reg_64 (Rbx, Rax))))
  (Instruction (Xor (Reg_64_reg_64 (Rax, Rbx))))
  (Instruction (Sub (Reg_64_imm_a (Rcx, (Imm_int 1)))))
  (Instruction (Cmp (Reg_64_imm_a (Rcx, (Imm_int 1)))))
  (Instruction (Jne (Label fib_loop)))
  (LabelDecl exit)
  (Instruction (Mov (Reg_64_imm_a (Rdi, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rax, (Imm_int 60)))))
  (Instruction (Syscall (Nothing)))
