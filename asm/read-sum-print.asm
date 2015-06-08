
%% DESCRIPTION: read/number/N - sum S=1+2+...N - print S.

%% READ AN INTEGER N FROM THE INPUT CONSOLE, COMPUTE 1+2+3+...+N AND
%% PRINT THE RESULT TO THE OUTPUT CONSOLE.

#### PRINT THE MESSAGE `N=`
        @78
        D=A
        @SCREEN
        A=A+D
        M=1
        @61
        D=A
        @SCREEN
        A=A+D
        M=1
        
#### READ AN INTEGER IN R15 FROM THE KEYBOARD
        @R15
        M=0

#### <<< CANONICAL READ KEY
(READ_KBD)
        @KBD
        D=M
        @READ_KBD
        D; JEQ
#### >>> CANONICAL READ KEY
        
        ## PRINT READ CHAR
        @SCREEN
        A=D+A
        M=1
        ## IF THE KEY PRESSED IS NEWLINE THEN STOP
        @10
        D=D-A
        @END
        D; JEQ
        ## ... ELSE CONVERT THE KEY TO DIGIT
        @38
        D=D-A

        ## IF THE KEY PRESSED IS NOT A DIGIT THEN EXIT
        @EXIT
        D; JLT
        @10
        D=A-D
        @EXIT
        D; JLE
        @10
        D=A-D
        
        ## SAVE DIGIT IN R13
        @R13
        M=D
        ## R15 = R15 * 10
        @R15
        D=M
        D=D+M
        @R14
        M=D                     ## R14 = R15*2
        @R15
        D=M
        M=M+D
        D=M
        M=M+D
        D=M
        M=M+D
        D=M                    ## D = R15*8
        @R14
        M=M+D                  ## R14 = 10*R15
        D=M                    ## D = R14
        ## R15 = R15 + DIGIT
        @R13
        D=D+M
        @R15
        M=D
        ## READ NEXT DIGIT
        @READ_KBD
        0; JMP
(END)

#### computes the sum S = 1+2+ ... + R15
        @R15
        D=M

        @sum
        M=0
(LOOP$SUM)
        @sum
        M=M+D
        D=D-1
        @LOOP$SUM
        D;JGT        // if D>0 goto loop

#### PRINT THE MESSAGE `=>`
        @61
        D=A
        @SCREEN
        A=A+D
        M=1
        @62
        D=A
        @SCREEN
        A=A+D
        M=1
        
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

(EXIT)
