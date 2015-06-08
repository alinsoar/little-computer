
        @100
        D = A
        @i
        M=D          // i = 100
        @s
        M = 0        // s = 0
(LOOP)
        @i
        D=M          // D = i
        @s
        M=M+D        // s = s+i
        @i
        M=M-1        // i = i-1
        @LOOP
        D;JGT        // if D>0 goto loop
        @s
        D=M
(end)
        @end
        0;JMP
