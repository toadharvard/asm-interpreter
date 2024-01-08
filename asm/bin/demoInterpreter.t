  $ ./demoInterpreter.exe <<- EOF
  > section .text
  > global _start
  > _start:
  >    ; Инициализируем регистры для хранения предыдущих двух чисел Фибоначчи
  >    mov rcx, 10 ; Ищем 10-ое число Фибоначчи 
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
  >    cmp rcx, 0
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
  (Instruction (Mov (Reg_64_imm_a (Rcx, (Imm_int 10)))))
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
  (Instruction (Cmp (Reg_64_imm_a (Rcx, (Imm_int 0)))))
  (Instruction (Jne (Label fib_loop)))
  (LabelDecl exit)
  (Instruction (Mov (Reg_64_imm_a (Rdi, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rax, (Imm_int 60)))))
  (Instruction (Syscall (Nothing)))
  Registers:
  Eax: 0
  Ebp: 0
  Ebx: 0
  Ecx: 0
  Edi: 0
  Edx: 0
  Esi: 0
  Esp: 0
  Rax: 60
  Rbp: 0
  Rbx: 55
  Rcx: 0
  Rdi: 0
  Rdx: 0
  Rsi: 0
  Rsp: 0
  XMM Registers:
  Xmm0: (0, 0)
  Xmm1: (0, 0)
  Xmm2: (0, 0)
  Xmm3: (0, 0)
  Xmm4: (0, 0)
  Xmm5: (0, 0)
  Xmm6: (0, 0)
  Xmm7: (0, 0)
  Stack: 
  Label to jump: None
  Zero flag: 1

  $ ./demoInterpreter.exe <<- EOF
  > ; Считает скалярное произведение двух векторов (1, 2) и (3, 4) в xmm0 и xmm1
  > ; Результат кладет в rdx
  > global redefine_start_point
  > section .text
  > redefine_start_point:
  >    mov rbx, 1
  >    mov rcx, 2
  >    pinsrq xmm0, rbx, 1
  >    pinsrq xmm0, rcx, 0
  >    mov rbx, 3
  >    mov rcx, 4
  >    pinsrq xmm1, rbx, 1
  >    pinsrq xmm1, rcx, 0
  > 
  >    mulpd xmm0, xmm1
  >    movapd xmm2, xmm0
  >    punpckhqdq xmm2, xmm2
  >    addpd xmm0, xmm2
  >    movq rdx, xmm0
  > exit:
  >    ; Завершение программы и возврат значения в rax
  >    mov rdi, 0 ; Код возврата
  >    mov rax, 60 ; syscall номер для exit
  >    syscall
  > EOF
  (Directive (Global redefine_start_point))
  (Directive (Section .text))
  (LabelDecl redefine_start_point)
  (Instruction (Mov (Reg_64_imm_a (Rbx, (Imm_int 1)))))
  (Instruction (Mov (Reg_64_imm_a (Rcx, (Imm_int 2)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm0, Rbx, (Imm_int 1)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm0, Rcx, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rbx, (Imm_int 3)))))
  (Instruction (Mov (Reg_64_imm_a (Rcx, (Imm_int 4)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm1, Rbx, (Imm_int 1)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm1, Rcx, (Imm_int 0)))))
  (Instruction (Mulpd (Reg_128_reg_128 (Xmm0, Xmm1))))
  (Instruction (Movapd (Reg_128_reg_128 (Xmm2, Xmm0))))
  (Instruction (Punpckhqdq (Reg_128_reg_128 (Xmm2, Xmm2))))
  (Instruction (Addpd (Reg_128_reg_128 (Xmm0, Xmm2))))
  (Instruction (Movq (Reg_64_reg_128 (Rdx, Xmm0))))
  (LabelDecl exit)
  (Instruction (Mov (Reg_64_imm_a (Rdi, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rax, (Imm_int 60)))))
  (Instruction (Syscall (Nothing)))
  Registers:
  Eax: 0
  Ebp: 0
  Ebx: 0
  Ecx: 0
  Edi: 0
  Edx: 0
  Esi: 0
  Esp: 0
  Rax: 60
  Rbp: 0
  Rbx: 3
  Rcx: 4
  Rdi: 0
  Rdx: 11
  Rsi: 0
  Rsp: 0
  XMM Registers:
  Xmm0: (6, 11)
  Xmm1: (3, 4)
  Xmm2: (3, 3)
  Xmm3: (0, 0)
  Xmm4: (0, 0)
  Xmm5: (0, 0)
  Xmm6: (0, 0)
  Xmm7: (0, 0)
  Stack: 
  Label to jump: None
  Zero flag: 0
