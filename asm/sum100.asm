#### DESCRIPTION: compute the primitive recursive function 1+2+...+100.

        @100
        D=A

        @sum
        M=0
(LOOP$SUM)
        @sum
        M=M+D
        D=D-1
        @LOOP$SUM
        D;JGT        // if D>0 goto loop

#### PRINT the integer from @sum

        ## R15=10,000*R14 + rest
        @sum
        D=M
        @R14
        M=0
        @R13
        M=0
(sub10K)
        @10000
        D=D-A
        @stop10K
        D; JLT
        @R14
        M=M+1
        @sub10K
        0; JMP
(stop10K)
        @10000
        D=D+A
        @R15
        M=D                     ## R15=rest
        @R14
        D=M
        @R13
        MD=M+D
        @skipprint5th
        D; JEQ
        @R14
        D=M
        @48
        D=A+D
        @SCREEN
        A=D+A
        M=1
(skipprint5th)

	## R15=1,000*R14 + rest
        @R15
        D=M
        @R14
        M=0
(sub1000)
        @1000
        D=D-A
        @stop1000
        D; JLT
        @R14
        M=M+1
        @sub1000
        0; JMP
(stop1000)
        @1000
        D=D+A
        @R15
        M=D                     ## R15=rest
        @R14
        D=M
        @R13
        MD=M+D
        @skipprint4th
        D; JEQ
        @R14
        D=M
        @48
        D=A+D
        @SCREEN
        A=D+A
        M=1
(skipprint4th)
        
	## R15=100*R14 + rest
        @R15
        D=M                     ## R15=rest
        @R14
        M=0
(sub100)
        @100
        D=D-A
        @stop100
        D; JLT
        @R14
        M=M+1
        @sub100
        0; JMP
(stop100)
        @100
        D=D+A
        @R15
        M=D
        @R14
        D=M
        @R13
        MD=M+D
        @skipprint3rd
        D; JEQ
        @R14
        D=M
        @48
        D=A+D
        @SCREEN
        A=D+A
        M=1
(skipprint3rd)
        
	## R15=10*R14 + rest
        @R15
        D=M                     ## R15=rest
        @R14
        M=0
(sub10)
        @10
        D=D-A
        @stop10
        D; JLT
        @R14
        M=M+1
        @sub10
        0; JMP
(stop10)
        @10
        D=D+A
        @R15
        M=D
        @R14
        D=M
        @R13
        MD=M+D
        @skipprint2nd
        D; JEQ
        @R14
        D=M
        @48
        D=A+D
        @SCREEN
        A=D+A
        M=1
(skipprint2nd)
        
	## R15
        @R15
        D=M
        @48
        D=A+D
        @SCREEN
        A=D+A
        M=1

