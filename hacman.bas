10 EXTEND
20 ! Hacman.Bas - Clone of famous arcade game &
   ! David Moisan, 7/31/2012
30 ! NOTE:  This program (HACMAN.BAS) is the user interface &
   ! and front end to HACMAG.BAS, which is the main game engine
40   DEBUG%=0%
100  ON ERROR GOTO 9500
495  ! Functions
510  ESKB$=CHR$(27) ! Default keyboard escape character
520  DEF FNCL$=CHR$(155)+"[2J" !FNCL$ - Clear screen
530  DEF FNNC$=CHR$(155)+"(B" !FNNC$ - Normal characters
540  DEF FNLD$=CHR$(155)+"(0" !FNLD$ - Line drawing characters
559  !FNAD$(Row,Column) Direct Cursor Addressing
560  DEF FNAD$(R%,C%)=CHR$(155)+"["+NUM1$(R%)+";"+NUM1$(C%)+"H"
570  !FNHM$ Place Cursor at Home Position
580  DEF FNHM$ = CHR$(155)+"[H"
590  DEF FNSETDE(DELAY) ! Calculate delay for busy-wait loop
600  DELTM = TIME(0%)
610  DELAY=0
620  DELAY=DELAY+1 UNTIL (TIME(0%)-DELTM)>=2 ! Calculate delay over 1 seconds
630  FNSETDE = DELAY/2 ! and return one second's worth
640  FNEND
700  DEF FNDELAY(SEC) ! Delay for time in seconds (using DELAY var returned by FNSETDE)
710  FOR I=1 TO DELAY*SEC \ NEXT I
720  FNEND
750  DEF FNMOD(X,M) = (X/M-INT(X/M))*M  ! FNMOD: Calculate X mod M
799  ! FNCENT$ - Center string and print at specified row
800  DEF FNCENT$(ROW%,STR$)
810  CENT%=40-(INT(LEN(STR$)+0.5)/2) ! Compute centered location
820  FNCENT$ = FNAD$(ROW%,CENT%)+STR$
830  FNEND
995 ! * Main code start
1000 GOSUB 4500 ! Load high score table
1010 XX$=SYS(CHR$(8%)+"") !Clear any old scores from core common
1200 DELAY= FNSETDE(DELAY)! Calculate delay
1300 CMKY$="NHSRCQX" ! Commands:  Help, high (S)core, (Q)uit, (E)xit, (N)ew game: New game, (R)eset high scores
1320 OPEN "KB:" FOR INPUT AS FILE 1%
1330 XX$ = SYS(CHR$(6%)+CHR$(-7%))! Control-C trap
1340 XX%=SPEC%(2%,0%,1%,2%)!Re-enable echo
1410 GOSUB 2000
1420 AL%=1 ! Attract loop counter
1498 ! ** Input loop **
1500 WAIT 10
1510 PRINT FNNC$;FNAD$(23,21);"Enter N to start new game, H for help:";
1520 INPUT I$
1540 I%=INSTR(1,CMKY$,CVT$$(I$,32%)) \ IF I%=0 THEN 1600! Skip if no recognizable input
1550 ON I% GOTO 1900,1700,1750,1800,1730,9800,9800 ! Help, High score table, Quit/Exit,New game/enter, Reset score
1600 ON AL% GOSUB 5500, 6000, 4000, 2000
1610 AL%=AL%+1 \ IF AL%>4 THEN AL%=1
1690 GOTO 1500 ! End input loop
1695 ! Display help part 1
1700 GOSUB 5500 \ GOTO 1500
1719 ! Display help part 2
1720 GOSUB 6000 \ GOTO 1500
1729 ! Credits
1730 GOSUB 6500
1740 GOTO 1500
1749 ! * High Score table
1750 GOSUB 4000
1770 GOTO 1500
1779 ! * Reset high score table
1800 PRINT FNCL$;FNAD$(23,25);"Really reset high scores (y/N):";
1810 INPUT R$ \ IF R$="Y" THEN GOSUB 5000
1890 GOTO 1500
1895 ! Start new game
1900 XX$=SYS(CHR$(8%)+"") ! Clear core common
1910 IF NOT DEBUG% THEN CHAIN "HACMAG"
1920 IF DEBUG% THEN PRINT FNNC$;FNAD$(22,0);"** Game invoked"
1940 GOTO 1500
1995 ! * Display title
2000 PRINT FNCL$;FNNC$;
2010 PRINT FNAD$(1,22);"David Moisan's Retrogaming Presents:"
2020 PRINT FNLD$
2025 PRINT\PRINT\PRINT\PRINT
2030 PRINT TAB(18);"w   w                    lqk lqk            "
2040 PRINT TAB(18);"x   x                    x x x x            "
2050 PRINT TAB(18);"x   x                    x x x x            "
2060 PRINT TAB(18);"tqqqu  tqqk  lqqu        x mwj x  tqqk  lqqk"
2070 PRINT TAB(18);"x   x  lqqu  x     tqqu  x  x  x  lqqu  x  x"
2080 PRINT TAB(18);"x   x  x  x  x           x  x  x  x  x  x  x"
2090 PRINT TAB(18);"v   v  mqqj  mqqu        v  v  v  mqqj  v  v"
2100 PRINT
2200 PRINT
2210 PRINT FNAD$(16,20);"...................................... O"
2250 FOR I%=20 TO 60
2260 PRINT FNAD$(15,I%);" l/";
2270 PRINT FNAD$(16,I%);" m\";
2280 XX=FNDELAY(0.25)
2290 NEXT I%
3480 PRINT FNNC$;
3490 RETURN
3995 ! * Display high score
4000 PRINT FNCL$;FNNC$ ! Clear screen, normal characters
4010 PRINT FNCENT$(0,"*** Top Hackers ***")
4015 PRINT\PRINT
4020 FOR I%=1 TO 10
4030 IF HIGHSC(I%)=0 THEN 4090  ! Skip blank scores
4040 PRINT TAB(20);\PRINT USING "###,###,###    \                      \",HIGHSC(I%),PLAYER$(I%)
4090 NEXT I%
4240 RETURN
4495 ! * Load high score table
4500 DIM HIGHSC(10),PLAYER$(10)
4510 I%=1
4520 OPEN "HACMAN.SCR" AS FILE 2%
4530 INPUT #2, HIGHSC(I%),PLAYER$(I%)
4540 IF I%<=10 THEN I%=I%+1\ GOTO 4530
4550 CLOSE 2%
4560 IF I%=1 THEN GOSUB 5000 ! Re-create high scores if empty
4590 RETURN
4995 ! * Reset high score table
5000 RESTORE
5050 FOR I%=1 TO 10
5060 READ HIGHSC(I%),PLAYER$(I%)
5070 NEXT I%
5080 DATA 100000,"Dave M.",90000,"Chris H.",80000,"John O.",70000,"Randy M.",60000,"Peter G.", 50000, "Dave I.", 40000, "Tom R.",300
00,"Sal R.",20000,"Casey D.",10000,"Patrick K."
5100 OPEN "HACMAN.SCR" FOR OUTPUT AS FILE 2%
5120 FOR I%=1 TO 10
5130 PRINT #2,HIGHSC(I%);",";PLAYER$(I%)
5140 NEXT I%
5240 RETURN
5495 !* Display help
5500 PRINT FNCL$;FNNC$
5501 !Presenting the ghosts
5502 !Ink!
5510 PRINT FNAD$(2,10);"Presenting..."
5550 FOR I%=5 TO 15
5560 PRINT FNAD$(4,I%);" ##";
5570 PRINT FNAD$(5,I%);" --";
5580 XX=FNDELAY(0.40)
5590 NEXT I%
5600 PRINT FNAD$(4,20);"Ink!"
5610 SLEEP 1
5620 FOR I%=5 TO 15
5630 PRINT FNAD$(7,I%);" **"; \PRINT FNAD$(8,I%);" --";
5640 XX=FNDELAY(0.40)
5650 NEXT I%
5660 PRINT FNAD$(7,20);"Blink!"
5670 SLEEP 1
5680 FOR I%=5 TO 15
5690 PRINT FNAD$(10,I%);" @@"; \ PRINT FNAD$(11,I%);" --";
5700 XX=FNDELAY(0.40)
5710 NEXT I%
5720 SLEEP 1
5730 PRINT FNAD$(10,20);"Pink!"
5740 FOR I%=5 TO 15
5750 PRINT FNAD$(13,I%);" %%"; \PRINT FNAD$(14,I%);" --";
5760 XX=FNDELAY(0.40)
5770 NEXT I%
5780 PRINT FNAD$(13,20);"Sal!"
5790 SLEEP 3
5800 PRINT FNLD$;
5810 PRINT FNAD$(16,10);"                             \O         00"
5820 PRINT FNAD$(17,10);"     ..........     O        O          ^^"
5830 SLEEP 1
5850 H1$=" l/" \ H2$=" m\"
5900 FOR I%=10 TO 20 \ PRINT FNAD$(16,I%);H1$;FNAD$(17,I%);H2$; \ XX=FNDELAY(0.40) \ NEXT I%
5905 SLEEP 1
5910 PRINT FNAD$(18,17);"10 PTS";
5920 FOR I%=21 TO 30\PRINT FNAD$(16,I%);H1$;FNAD$(17,I%);H2$; \ XX=FNDELAY(0.40) \ NEXT I%
5930 PRINT FNAD$(18,27);"50 PTS";
5940 FOR I%=31 TO 40\PRINT FNAD$(16,I%);H1$;FNAD$(17,I%);H2$; \ XX=FNDELAY(0.40) \ NEXT I%
5950 PRINT FNAD$(18,38);"?????";
5960 FOR I%=41 TO 50\PRINT FNAD$(16,I%);H1$;FNAD$(17,I%);H2$;\XX=FNDELAY(0.40)\NEXT I%
5970 PRINT FNAD$(18,48);"?????";
5980 PRINT FNNC$;FNAD$(20,29);\PRINT USING "Bonus life every ##,###",20000
5990 RETURN
5995 !Display help part 2
6000 PRINT FNCL$;FNNC$
6020 PRINT FNAD$(2,30);"** Keyboard Help **"
6030 PRINT FNAD$(4,27);"* N starts new game"
6040 PRINT FNAD$(6,27);"* Arrow keys move Hac-Man"
6050 PRINT FNAD$(8,27);"* Q quits game"
6070 PRINT FNAD$(10,27);"* S displays high scores"
6080 PRINT FNAD$(12,27);"* R resets high score table"
6090 PRINT FNAD$(14,27);"* H displays this screen"
6100 PRINT FNAD$(16,27);"* C displays credits"
6190 RETURN
6495 ! * Credits
6500 PRINT FNCL$;FNNC$
6510 PRINT FNCENT$(2,"** Credits **")
6515 SLEEP 1
6520 PRINT FNCENT$(4,"Lead hacker, remake and original")
6530 PRINT FNCENT$(5,"David Moisan")
6540 SLEEP 1
6550 PRINT FNCENT$(8,CHR$(34)+"Computer Gang"+CHR$(34)+" at Salem High School")
6560 SLEEP 1
6570 PRINT FNCENT$(10,"Randy Moisan")
6580 SLEEP 1
6590 PRINT FNCENT$(12,"David Moisan")
6600 SLEEP 1
6610 PRINT FNCENT$(14,"Chris Haight")
6620 SLEEP 1
6630 PRINT FNCENT$(16,"Peter Georgelas")
6640 SLEEP 1
6650 PRINT FNCENT$(18,"David Irish")
6660 SLEEP 1
6670 PRINT FNCENT$(20,"Special thanks to Thomas Risoldi,")
6680 PRINT FNCENT$(21,"the past math chair, Salem High School")
6690 SLEEP 6
6700 PRINT FNCL$;FNNC$
6710 PRINT FNCENT$(2,"** Technical Credits **")
6720 SLEEP 1
6730 PRINT FNCENT$(5,"Emulation technology")
6740 PRINT FNCENT$(6,"SIMH")
6750 PRINT FNCENT$(7,"simh.trailing-edge.com")
6760 SLEEP 1
6770 PRINT FNCENT$(9,"Documentation")
6780 PRINT FNCENT$(10,"www.bitsavers.org")
6790 SLEEP 1
6800 PRINT FNCENT$(12,"Terminal emulation")
6810 PRINT FNCENT$(13,"Tera Term")
6820 PRINT FNCENT$(14,"ttssh2.sourceforge.jp")
6990 SLEEP 10
6995 RETURN
7995 !Halftime display entry point
8000 GM$=SYS(CHR$(7%)) ! Get core common string and hold on to it
8010 DELAY = FNSETDE(0)
8020 PRINT FNCL$;FNLD$ ! Clear screen
8030 FOR I%=1 TO 70 ! Hac-Man left-to-right animation
8040 PRINT FNAD$(12,I%);" @@    l/ ";
8050 PRINT FNAD$(13,I%);" --    m\ ";
8060 XX=FNDELAY(0.30)
8070 NEXT I%
8080 SLEEP 1 ! Pause one second
8090 PRINT FNCL$;
8100 FOR I%=70 TO 1 STEP -1 ! Hac-Man right to left animation
8110 PRINT FNAD$(12,I%);" 00    \k ";
8120 PRINT FNAD$(13,I%);" ^^    /j ";
8130 XX=FNDELAY(0.30)
8150 NEXT I%
8160 SLEEP 1
8170 PRINT FNCL$;FNNC$;
8210 IF NOT DEBUG% THEN CHAIN "HACMAG"
8220 STOP  !Should not reach this point, but in case...
8495 !Game over entry point
8500 ON ERROR GOTO 9500
8505 GM$=SYS(CHR$(7%))
8510 IF LEFT(GM$,6)<>"HACMAN" GOTO 1000 ! If no score passed, resume attract loop
8512 IF DEBUG% THEN PRINT FNNC$;FNCL$;"Core common=";GM$\STOP
8519 !Validate string
8520 SCORE=CVT$F(MID(GM$,7,LEN(CVTF$(0))))
8530 LEV%=CVT$%(MID(GM$,7+LEN(CVTF$(0)),2))
8610 PRINT FNCL$
8620 PRINT FNAD$(11,34);"lqqqqqqqqqqqqqqqqqqqk"
8630 PRINT FNAD$(12,34);"x G A M E   O V E R x"
8640 PRINT FNAD$(13,34);"mqqqqqqqqqqqqqqqqqqqj"
8645 PRINT FNNC$;
8650 SLEEP 2
8670 GOSUB 4500 !Read high score table
8700 NHS%=0 ! \New high score entry
8710 FOR I%=1 TO 10
8720 IF HIGHSC(I%) > SCORE OR NHS%<>0 THEN 8740
8730 NHS%=I%
8740 NEXT I%
8750 IF NHS%=0 THEN 9080 !No new high score
8800 PRINT FNAD$(19,10);"High score!"
8810 XX%=SPEC%(2%,0%,0%,2%)!Re-enable echo
8820 PRINT FNAD$(20,10);"Enter your name: ";
8830 INPUT PLNAME$
8840 IF LEN(PLNAME$)>20 THEN PLNAME$=LEFT(PLNAME$,20)
8900 FOR I%=10 TO NHS% STEP -1
8910 HIGHSC(I%)=HIGHSC(I%-1)\PLAYER$(I%)=PLAYER$(I%-1)
8920 NEXT I%
8930 HIGHSC(NHS%)=SCORE \ PLAYER$(NHS%)=PLNAME$
8950 OPEN "HACMAN.SCR" FOR OUTPUT AS FILE 2%
8960 FOR I%=1 TO 10
8970 PRINT #2,HIGHSC(I%);",";PLAYER$(I%)
8980 NEXT I%
8990 CLOSE 2%
9080 GOSUB 4000 !Display high scores
9090 GOTO 1200 !Resume attract loop
9498 !* Error handling
9500 IF ERR=15 AND ERL=1520 THEN RESUME 1600 !No KB input error, resume loop
9510 IF ERR=15 AND ERL=1810 THEN RESUME 1890 !and also in high score reset
9520 IF ERR=11 AND ERL=4530 THEN RESUME 4550 !TRAP END OF FILE ERROR IN HIGHSCORE
9530 IF ERR=28 THEN 9800 !Control-C Trap:  End game
9540 PRINT FNNC$ !Reset normal characters
9550 ON ERROR GOTO 0!Enable RSTS error handling
9560 STOP
9798 !* Exit program
9800 PRINT FNNC$;FNCL$ ! Reset ANSI normal character set and clear screen
9820 CLOSE 1%
9990 END
