

# DESCRIPTION

The Little Computer is a tool for creating computers.
I have implemented it with the purpose of building the Hack Computer
that is defined in the \`TEoCS\` book, "The Elements of
Computing Systems", MIT press, and taught in the course "From NAND to Tetris".

The semantics of the Hardware Description Language that the Little
Computer implemented is defined in the Appendix A of the TEoCS book.

The only difference between the semantics defined in the TEoCS and the
current implementation is in Output. See the INPUT AND OUTPUT.

The development of the little computer took exactly 20 days of working a
few hours each day.  I built it after I discovered the Nand2Tetris
course on coursera.  In the first 3 weeks I quickly studied the
materials and in the last 3 weeks I built the little computer.


# INPUT AND OUTPUT

The output is different from the output defined in the TEoCS.

The Little Computer does not have a graphical output, it has only a
simple console that can print ASCII characters only.

In the TEoCS implementation of the output one plots graphical pixels.
To draw the pixel X,Y one needs to do so: SCREEN[X][Y]=1 (formula is
abstracted modulo bus width).

In the Little Computer, in order to print the character with the ascii
code A we do simply so: SCREEN[A]=1.  The useful screen memory has
only 128 locations. After such an assembler command is executed, at
the next clock tick the action associated with the electric wire
connected to the display will print the character.

This is the only semantic difference between the TEoCS definition
and the current implementation.

Concerning the input, the operating system is supposed to provide the
sound GETCHAR function that should be used by the programmer.  The
hack programmer who wants to read keys from the keyboard should know
how the hack computer works in its internals in order to successfully
write a getchar function.  See the example code and the comments from
\`sim\` in order to understand the logic behind.


# THE LAYERS OF LANGUAGES

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">FILE</th>
<th scope="col" class="org-left">IN-LANG</th>
<th scope="col" class="org-left">OUT-LANG</th>
<th scope="col" class="org-left">DESCRIPTION</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">*.hdl</td>
<td class="org-left">ALGEBRAIC EQUATIONS</td>
<td class="org-left">HDL</td>
<td class="org-left">hardware description language</td>
</tr>

<tr>
<td class="org-left">*.hdl</td>
<td class="org-left">HDL</td>
<td class="org-left">HDL BYTECODE</td>
<td class="org-left">semantic check of hdl files</td>
</tr>

<tr>
<td class="org-left">*.hwc</td>
<td class="org-left">HDL BYTECODE</td>
<td class="org-left">HWS</td>
<td class="org-left">modularize the hardware structures</td>
</tr>

<tr>
<td class="org-left">sim</td>
<td class="org-left">HWS; HACK MICROCODE</td>
<td class="org-left">PROPAGATORS</td>
<td class="org-left">simulator of the electrical signal flow</td>
</tr>

<tr>
<td class="org-left">lexer</td>
<td class="org-left">REGEX COMBINATORS, RAW CODE</td>
<td class="org-left">TOKEN STREAM</td>
<td class="org-left">tokenize an input in function of regexps</td>
</tr>

<tr>
<td class="org-left">hack-lexical</td>
<td class="org-left">REGULAR EXPRESSIONS</td>
<td class="org-left">REGEX COMBINATORS</td>
<td class="org-left">define hack token syntax</td>
</tr>

<tr>
<td class="org-left">hack-phrase-struct</td>
<td class="org-left">AUTOMATA, HACK TOKENS</td>
<td class="org-left">HACK MICROCODE</td>
<td class="org-left">called by hack assembler</td>
</tr>
</tbody>
</table>


# HOW TO USE IT


## 1. compile the .hdl equations with

    make hwc

This will create the .hw files in ./hw directory. One can split the
whole computer in any modules we want by inserting them in
MODULE-LIST.  Each module in this list is compiled as a unit and can
be either expanded in smaller atoms in the simulator or can be
replaced by a software simulator within sim.

Before compiling the compiler, one can modify MODULE-LIST to set the
circuits that are explicitly exposed during compilation.  The only
circuit that is hardcoded is the NAND.  So, if you need to expand
everything into NANDs, set this variable to the empty list.

If you want to test a circuit defined in some HDL, insert it in this
list and run the simulator against that circuit (eval 'Circuit &#x2026;),
instead of (eval 'Computer &#x2026;).  Like this, the simulator can be
used to simulate any computer, not only the Hack Computer, that was
the initial target of this project.


## 2. compile the hack programs with

    make hack

This will create the binary files in Hack format in ./bin directory.


## 3. see the list of available hack programs doing so:

    make sim


## 1.2.3.You can also execute all the steps 1-3 once just by typing:

    make rebuild


## 4. execute the hack program \`collatz\` (<https://oeis.org/A008884>) so:

    make sim HACK=collatz


## 5. example output

&#x2014; check the HDL semantically and build the modules from MODULE-LIST

    
    ~~ make hwc
    creating the directory ./bin/
    creating the directory ./hw/
    Loaded ALU, Add16, And, And16, Bit, CPU, Computer, CondNeg16, DFF, DLatch, DMux, DMux4Way, DMux8Way, FullAdder, HalfAdder, Inc16, Keyboard, Memory, Mux, Mux16, Mux4Way16, Mux8Way16, Nand, Not, Not16, Or, Or16, Or8Way, PC, RAM16K, RAM4K, RAM512, RAM64, RAM8, ROM32K, RSFlipFlop, RSLatch, Register, Screen, Xor, Xor16, Zero16, total 42 gates defined.
     Compiling Computer
     Longest definition: 12
    Saving compiled modules: Mux; Bit; RAM8; Computer; RAM64; RAM512; Register; Add16; Inc16; RAM4K; CPU; RAM16K;

&#x2014; compile the hdl computer to a vectorial and symbolic format

&#x2014; compile the user applications written in assembly to microcode

    
    ~~  make hack
    creating the directory ./bin/
    creating the directory ./hw/
    Compile collatz
    Compile echo
    Compile print-input
    Compile read-sum-print
    Compile sum100

&#x2014; The Collatz program reads an integer from the input and displays the values of the collatz function.

    ~~ make sim HACK=collatz
    racket sim.rkt ./bin//collatz.hack
     Loading HWS abstractions ............ done.
     Load the microcode...
     Build the datapath...
     REG D ok.
     REG A ok.
     REG M ok.
     REG PC ok.
     RAM activated on 14 bits.
     Screen activated.
     Keyboard activated.
     Input console settings saved.
     Input console commuted in raw mode.
     ROM installed.
     The microcode has a length of 396 instructions.
     Reset... reset complete.
     The CLOCK is connected to 195 wires.
     total 1639 nands.
     total 1734 wires.
     total 2 registers expanded as nands.
     total 3103 propagator actions installed.
     Starting the HACK microcode execution.
    --
    N=27
     1.82 2.41 3.124 4.62 5.31 6.94 7.47 8.142 9.71 10.214
     11.107 12.322 13.161 14.484 15.242 16.121 17.364 18.182 19.91 20.274
     21.137 22.412 23.206 24.103 25.310 26.155 27.466 28.233 29.700 30.350
     31.175 32.526 33.263 34.790 35.395 36.1186 37.593 38.1780 39.890 40.445
     41.1336 42.668 43.334 44.167 45.502 46.251 47.754 48.377 49.1132 50.566
     51.283 52.850 53.425 54.1276 55.638 56.319 57.958 58.479 59.1438 60.719
     61.2158 62.1079 63.3238 64.1619 65.4858 66.2429 67.7288 68.3644 69.1822 70.911
     71.2734 72.1367 73.4102 74.2051 75.6154 76.3077 77.9232 78.4616 79.2308 80.1154
     81.577 82.1732 83.866 84.433 85.1300 86.650 87.325 88.976 89.488 90.244
     91.122 92.61 93.184 94.92 95.46 96.23 97.70 98.35 99.106 100.53
     101.160 102.80 103.40 104.20 105.10 106.5 107.16 108.8 109.4 110.2
     111.1
    --
     FINISHED
     fugit irreparabile tempus 47977
     Console restored.

&#x2014; list the available applications

    
    ~~ make sim
    To execute the hack program PROG do so:
    \tmake sim HACK=PROG
    PROG can be one of
            collatz          The Syracuse algorithm.
            echo             classical ECHO.
            print-input      read-print-loop.
            read-sum-print   read/number/N - sum S=1+2+...N - print S.
            sum100           compute the primitive recursive function 1+2+...+100.

&#x2014; compute 1+2+3+&#x2026;+N

    
    ~~ make sim HACK=read-sum-print
    racket sim.rkt ./bin//read-sum-print.hack
     Loading HWS abstractions ............ done.
     Load the microcode...
     Build the datapath...
     REG D ok.
     REG A ok.
     REG M ok.
     REG PC ok.
     RAM activated on 14 bits.
     Screen activated.
     Keyboard activated.
     Input console settings saved.
     Input console commuted in raw mode.
     ROM installed.
     The microcode has a length of 201 instructions.
     Reset... reset complete.
     The CLOCK is connected to 195 wires.
     total 1639 nands.
     total 1734 wires.
     total 2 registers expanded as nands.
     total 3103 propagator actions installed.
     Starting the HACK microcode execution.
    --
    N=100
    =>5050
    --
     FINISHED
     fugit irreparabile tempus 2210
     Console restored.

&#x2014; an infinite loop that echoes the input after pressing return.

&#x2014; Ctrl-c to stop

    
    ~~ make sim HACK=echo
    racket sim.rkt ./bin//echo.hack
     Loading HWS abstractions ............ done.
     Load the microcode...
     Build the datapath...
     REG D ok.
     REG A ok.
     REG M ok.
     REG PC ok.
     RAM activated on 14 bits.
     Screen activated.
     Keyboard activated.
     Input console settings saved.
     Input console commuted in raw mode.
     ROM installed.
     The microcode has a length of 48 instructions.
     Reset... reset complete.
     The CLOCK is connected to 195 wires.
     total 1639 nands.
     total 1734 wires.
     total 2 registers expanded as nands.
     total 3103 propagator actions installed.
     Starting the HACK microcode execution.
    --
    Hello World!
    Hello World!
    
    --
     INTERRUPTED
     fugit irreparabile tempus 10050
     Console restored.

