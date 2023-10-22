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
  [(Directive (Section ".text")); (Label "fibonachch");
    (Instruction (InstrOperand1 (Push, (Reg (I64 Rbp)))));
    (Instruction (InstrOperand2 (Mov, (Reg (I64 Rbx)), (Imm (NumVal 42)))));
    (Instruction (InstrOperand2 (Mov, (Reg (I64 Rax)), (Imm (NumVal 0)))));
    (Instruction (InstrOperand2 (Mov, (Reg (I64 Rcx)), (Imm (NumVal 1)))));
    (Instruction (InstrOperand2 (Cmp, (Reg (I64 Rbx)), (Imm (NumVal 1)))));
    (Instruction (InstrOperand1 (Je, (LableRef "fibonachchEnd"))));
    (Instruction (InstrOperand2 (Cmp, (Reg (I64 Rbx)), (Imm (NumVal 2)))));
    (Instruction (InstrOperand1 (Je, (LableRef "fibonachchTwo"))));
    (Instruction (InstrOperand2 (Sub, (Reg (I64 Rbx)), (Imm (NumVal 1)))));
    (Label "fibonachchStart");
    (Instruction (InstrOperand2 (Sub, (Reg (I64 Rbx)), (Imm (NumVal 1)))));
    (Instruction (InstrOperand2 (Xor, (Reg (I64 Rax)), (Reg (I64 Rcx)))));
    (Instruction (InstrOperand2 (Xor, (Reg (I64 Rcx)), (Reg (I64 Rax)))));
    (Instruction (InstrOperand2 (Xor, (Reg (I64 Rax)), (Reg (I64 Rcx)))));
    (Instruction (InstrOperand2 (Add, (Reg (I64 Rax)), (Reg (I64 Rcx)))));
    (Instruction (InstrOperand2 (Cmp, (Reg (I64 Rbx)), (Imm (NumVal 0)))));
    (Instruction (InstrOperand1 (Je, (LableRef "fibonachchEnd"))));
    (Instruction (InstrOperand1 (Jmp, (LableRef "fibonachchStart"))));
    (Label "fibonachchTwo");
    (Instruction (InstrOperand2 (Mov, (Reg (I64 Rax)), (Imm (NumVal 1)))));
    (Label "fibonachchEnd");
    (Instruction (InstrOperand1 (Pop, (Reg (I64 Rbp)))));
    (Instruction (InstrOperand0 Ret))]
