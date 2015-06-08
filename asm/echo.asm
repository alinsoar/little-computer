
%% DESCRIPTION: classical ECHO.

%% This is the classical *ECHO* program.  It reads the keyboard's
%% input.  When newline is pressed it echoes the full line and starts
%% again the READLINE-PRINTLINE cycle.

(repeat$main)
        @buffer
        D=A
        @R0
        M=D
(KBD$LOOP)
        ## read the keyboard
        @KBD
        ## save the input key to D
        D=M
        @KBD$LOOP
        ## If no key was pressed then loop
        D; JEQ
        ## Print the pressed key to the output console
        @SCREEN
        A=D+A
        M=1
        ## good bye when C-d interrupts
        @4
        D=D-A
        @GOODBYE
        D; JEQ
        @4
        D=D+A
        ## grow line buffer
        @R0
        A=M
        M=D
        @R0
        M=M+1
        ## test for newline
        @10
        D=D-A
        @PRINT$LINE
        D; JEQ
        ## repeat the READ-PRINT LOOP
        @KBD$LOOP
        0;JMP
#### At the end of a line execute an echo of the whole line
(PRINT$LINE)
	@buffer
        D=A
        @R1
        M=D
(print$next$char)
        @R1
        A=M
        D=M
        @SCREEN
        A=D+A
        M=1
        @R1
        MD=M+1
        @R0
        D=D-M
        @print$next$char
        D; JLT
## restart the main loop forever
        @repeat$main
        0; JMP
(GOODBYE)
        @GOODBYE
        0; JMP
