
%% DESCRIPTION: read-print-loop.


%% This is a GETCHAR-PRINTCHAR cycle. It echoes the character that is
%% read from the keyboard and repeat this cycle forever and ever.

(LOOP)
        ## read the keyboard
        @KBD
        ## save the input key to D
        D=M
        @LOOP
        ## If no key was pressed then loop
        D; JEQ
        ## Print the pressed key to the output console
        @SCREEN
        A=D+A
        M=1
        @LOOP
        ## repeat the READ-PRINT LOOP
        0;JMP

