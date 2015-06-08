%% DESCRIPTION: The Syracuse algorithm.

## Paul Erdos offered $500 for the solution of Collatz conjecture.

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

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ The Syracuse algorithm starts now.
        @R15
        D=M
        @NUM
        M=D                     ;; save initial num
        @STEP
        M=0
	

(LOOP$COLLATZ)
        @NUM
        D=M                     ;; D=NUM

        @1
        D=D&A
        @EVEN$NUM
        D; JEQ
        @NUM
        D=M
        @NUM
        M=M+1
        M=M+D
        M=M+D

        @PRINT
        0; JMP
        
(EVEN$NUM)
        @NUMDIV2
        0; JMP
(CONTINUE$DIV2)
        @NUM
        M=D

        @PRINT
        0; JMP
        
(CONTINUE)
        @NUM
        D=M
        D=D-1
        @LOOP$COLLATZ
        D; JGT
        @EXIT
        0; JMP
        
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PRINT

(PRINT)
        @STEP
        M=M+1
        
        
        @32
        D=A
        @SCREEN
        A=A+D
        M=1
        @XXX
        D=A
        @R11
        M=D
        @STEP
        D=M
        @R15
        M=D
        @PRINT$NUM
        0; JMP
(XXX)
        @CONTINUE
        D=A
        @R11
        M=D
        @NUM
        D=M
        @R15
        M=D
        @PRINT$NUM$HEADER
        0; JMP
	
        
(PRINT$NUM$HEADER)
#### PRINT THE MESSAGE `=>`
        @46
        D=A
        @SCREEN
        A=A+D
        M=1
	
(PRINT$NUM)

        ## R15=10,000*R14 + rest
        @R15
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

        @R11
        A=M
        0; JMP
	
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DIV=NUM/2
(NUMDIV2)
        @DIV2
        M=0

        ;; BIT 1
        @NUM
        D=M
        @2
        D=D&A
        @DIVBIT2
        D; JEQ
        @1
        D=A
        @DIV2
        M=D|M
(DIVBIT2)

        ;; BIT 2
        @NUM
        D=M
        @4
        D=D&A
        @DIVBIT3
        D; JEQ
        @2
        D=A
        @DIV2
        M=D|M
(DIVBIT3)
        
        ;; BIT 3
        @NUM
        D=M
        @8
        D=D&A
        @DIVBIT4
        D; JEQ
        @4
        D=A
        @DIV2
        M=D|M
(DIVBIT4)

        ;; BIT 4
        @NUM
        D=M
        @16
        D=D&A
        @DIVBIT5
        D; JEQ
        @8
        D=A
        @DIV2
        M=D|M
(DIVBIT5)

        ;; BIT 5
        @NUM
        D=M
        @32
        D=D&A
        @DIVBIT6
        D; JEQ
        @16
        D=A
        @DIV2
        M=D|M
(DIVBIT6)

        ;; BIT 6
        @NUM
        D=M
        @64
        D=D&A
        @DIVBIT7
        D; JEQ
        @32
        D=A
        @DIV2
        M=D|M
(DIVBIT7)

        ;; BIT 7
        @NUM
        D=M
        @128
        D=D&A
        @DIVBIT8
        D; JEQ
        @64
        D=A
        @DIV2
        M=D|M
(DIVBIT8)

        ;; BIT 8
        @NUM
        D=M
        @256
        D=D&A
        @DIVBIT9
        D; JEQ
        @128
        D=A
        @DIV2
        M=D|M
(DIVBIT9)

        ;; BIT 9
        @NUM
        D=M
        @512
        D=D&A
        @DIVBIT10
        D; JEQ
        @256
        D=A
        @DIV2
        M=D|M
(DIVBIT10)

        ;; BIT 10
        @NUM
        D=M
        @1024
        D=D&A
        @DIVBIT11
        D; JEQ
        @512
        D=A
        @DIV2
        M=D|M
(DIVBIT11)

        ;; BIT 11
        @NUM
        D=M
        @2048
        D=D&A
        @DIVBIT12
        D; JEQ
        @1024
        D=A
        @DIV2
        M=D|M
(DIVBIT12)

        ;; BIT 12
        @NUM
        D=M
        @4096
        D=D&A
        @DIVBIT13
        D; JEQ
        @2048
        D=A
        @DIV2
        M=D|M
(DIVBIT13)

        ;; BIT 13
        @NUM
        D=M
        @8192
        D=D&A
        @DIVBIT14
        D; JEQ
        @4096
        D=A
        @DIV2
        M=D|M
(DIVBIT14)

        ;; BIT 14
        @NUM
        D=M
        @16384
        D=D&A
        @DIVBIT15
        D; JEQ
        @8192
        D=A
        @DIV2
        M=D|M
(DIVBIT15)

        @DIV2
        D=M
        @CONTINUE$DIV2
        0; JMP


(EXIT)
        
