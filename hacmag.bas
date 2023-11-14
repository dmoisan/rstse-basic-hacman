10 EXTEND
20 !HACMAG.BAS -- Main game engine, executed by HACMAN.BAS front end
50 DEBUG%=0%
100 IF DEBUG% THEN OPEN "HACMAN.PRF" FOR OUTPUT AS FILE 3%
1010 DEF FNCL$=CHR$(155)+"[2J" ! FNCL$: Clear screen
1050 DEF FNNC$=CHR$(155)+"(B" !FNNC$ - Normal characters
1060 DEF FNLD$=CHR$(155)+"(0" !FNLD$ - Line drawing characters
1095 !FNAD$(Row,Column) Direct Cursor Addressing
1100 DEF FNAD$(R%,C%)=CHR$(155)+"["+NUM1$(R%)+";"+NUM1$(C%)+"H"
1145 !FNHM$ Place Cursor at Home Position
1150 DEF FNHM$ = CHR$(155)+"[H"
1300 ESKB$=CHR$(27) !kb escape chr.
1360 CMKY$="Q "
1370 ARKY$=CHR$(0)+CHR$(0)+CHR$(0)+ESKB$+"[D"+ESKB$+"[A"+ESKB$+"[C"+ESKB$+"[B"
1500 !*** General Const Definitions ***
1550 MAXLF%=3 !Default lives
1560 EXTLF=20000!Extra life every (n) points
1570 FRTSC=100 !100 points for cherry (doubled for orange)
1580 DOTSC=10  !Dots are 10 points
1590 PWRSC=50  !Power pills are 50
1598 ! Object detect constants
1600 DOT%  =ASCII(".")! Dot
1610 EMPTY%=ASCII(" ")! Empty space
1620 PILL% =ASCII("O")! Power pill
2000 !*** Sprite Graphics ***
2010 DIM SP$(32),SPX%(16),SPY%(16),STX%(16),STY%(16),SDR%(16),SSDR%(16),SSTAT%(16),SV%(16)
2011 !SP$: graphic sprite (2 strings per each) &
 !SPX%,SPY%: Current Position &
 !STX%,STY%: Starting Position &
 !SDR%,SSDR%: Direction &
 !SSTAT%,SV% : Status, Motion Ctr.
2020 SP$(0)="lk" !Sprite 0 Hacman at rest
2030 SP$(1)="mj"\STX%(0)=40\STY%(0)=21 !Start position
2040 SP$(2)="\k" !Sprite 1 Hacman Left
2050 SP$(3)="/j"
2060 SP$(4)="\/" !Sprite 2 Hacman Up
2070 SP$(5)="mj"
2080 SP$(6)="l/" !Sprite 3 Hacman Right
2090 SP$(7)="m\"
2100 SP$(8)="lk" !Sprite 4 Hacman Down
2110 SP$(9)="/\"
2150 DIM REV%(4) !Reverse direction index.
2160 REV%(0)=0 !Still position returns itself
2161 REV%(1)=3 !Left position returns Right
2162 REV%(2)=4 !Up position returns Down
2163 REV%(3)=1 !Right position returns Left
2164 REV%(4)=2 !Down position returns Up
2200 !Sprite 5, 6, 7, 8 : Ghosts
2210 SP$(10)="##"\STX%(5)=40\STY%(5)=9\SSDR%(5)=1 !Ghost 1, Ink
2220 SP$(11)="--"
2230 SP$(12)="**"\STX%(6)=38\STY%(6)=12\SSDR%(6)=2 !Ghost 2, Blink
2240 SP$(13)="--"
2250 SP$(14)="@@"\STX%(7)=40\STY%(7)=12\SSDR%(7)=2 !Ghost 3, Pink
2260 SP$(15)="--"
2270 SP$(16)="%%"\STX%(8)=42\STY%(8)=12\SSDR%(8)=2 !Ghost 4, Sal
2280 SP$(17)="--"
2290 SP$(18)="00" !Sprite 9, Power Pill Ghost
2300 SP$(19)="^^"
2395 !Edible Fruit Sprites (note: Order is sp. 11, 10 and 12)
2400 SP$(22)="\O" !Sprite 11, Cherries
2410 SP$(23)="O " \STX%(10)=40\STY%(10)=18 !Same location for all fruits
2420 SP$(20)="' " !Sprite 10, Apple
2430 SP$(21)="()"
2432 SP$(24)="/\" !Sp. 12 Diamond
2436 SP$(25)="\/"
2440 SP$(26)="**"\SP$(27)="**" !Sprite 12: Hac-Man death sprite
2449 !Direction vector array
2450 DIM DX%(4),DY%(4)
2460 DX%(0)=0\DY%(0)= 0 !0 Still (no motion)
2470 DX%(1)=-1\DY%(1)=0 !1 Left (-x)
2480 DX%(2)=0\DY%(2)=-1 !2 Up (+y)
2490 DX%(3)=1\DY%(3)=0 !3 Right (+x)
2495 DX%(4)=0\DY%(4)=1 !4 Down (+y)
2499 !Collision detection and background redraw vector array
2500 DIM CX%(4,2),CY%(4,2)
2510 CX%(1,1)=-1\CY%(1,1)=0!Detect left side of sprite
2520 CX%(1,2)=-1\CY%(1,2)=1
2530 CX%(2,1)= 0\CY%(2,1)=-1!Detect up side of sprite
2540 CX%(2,2)= 1\CY%(2,2)=-1
2545 CX%(3,1)= 2\CY%(3,1)=0!Detect right side of sprite
2550 CX%(3,2)= 2\CY%(3,2)=1
2555 CX%(4,1)= 0\CY%(4,1)=2!Detect down side of sprite
2560 CX%(4,2)= 1\CY%(4,2)=2
2845 !Sprite indices
2850 MAXGH%=8% !Sprite index, last normal ghost
2860 GHOST%=5% !Sprite index, first normal ghost
2870 PGHOS%=9% !Sprite index, power pill ghost (all ghosts)
2880 FRUIT%=10 !Sprite index to fruit
2885 SPDEAD%=13 !HacMan death sprite
2890 MXSP%=13% !Maximum number of sprites defined
2895 ! *** Gamefield string definition ***
2950 DIM GFL$(22%)! GFL$ = Gamefield drawn on screen
2960 DIM GFL%(80%,24%) ! GFL%(x,y) = Gamefield object detection
2970 TLX%=1\TLY%=12\TRX%=77\TRY%=12 !Tunnel coords
3000 GFL$(1) ="lqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqk"
3010 GFL$(2) ="x O ....... .............. .............  ............ ..... ...... ...... O x"
3020 GFL$(3) ="x                                                                            x"
3030 GFL$(4) ="x .lqqqqqu. tqqqqqqqqqqqu. tqqqqqqqqqqu. tqqqqqqqqqqu. tqqqu. tqqu. tqqqqqk. x"
3040 GFL$(5) ="x .x ...... ..... ........ ........ .... .... ........ ...... ..... ..... x. x"
3050 GFL$(6) ="x .x                                                                      x. x"
3060 GFL$(7) ="x .x .lqqk. lqqk. tqqqqqu. tqqqqqu. tqqqqqqu. tqqqqqu. tqqqu. lqqk. lqqk. x. x"
3070 GFL$(8) ="x .x .x  x. x  x. ........ ........ ..    ... ........ ...... x  x. x  x. x. x"
3080 GFL$(9) ="x .x .x  x. x  x                                              x  x. x  x. x. x"
3090 GFL$(10)="v .v .mqqj. mqqj. tqqqqqu. tqqqqqk. l------k. lqqqqqu. tqqqu. mqqj. mqqj. v. v"
3100 GFL$(11)="  .  ...... ..... ........ ..... x. x      x. x. ..... ...... ..... .....  .  "
3110 GFL$(12)="                                 x. x      x. x                               "
3120 GFL$(13)="w .w .lqqk. lqqk. lqqqqqk. tqqk. v. mqqqqqqj. v. lqqu. lqqqk. lqqk. lqqk. w. w"
3130 GFL$(14)="x .x .x  x. x  x. x     x.  . x.  . .... ... ... x . . x   x. x  x. x  x. x. x"
3140 GFL$(15)="x .x .x  x. x  x. x     x     x                  x     x   x. x  x. x  x. x. x"
3150 GFL$(16)="x .x .mqqj. mqqj. mqqqqqvqqu. mqqqqqqqu. tqqqqqqqj. tqqvqqqj. mqqj. mqqj. x. x"
3160 GFL$(17)="x .x ...... ..... ........... .........  .......... ......... ..... ..... x. x"
3170 GFL$(18)="x .x                                                                      x. x"
3180 GFL$(19)="x  mqqqqqu. tqqqqqqqqqqqqqqu. tqqqqqqqqqqqqqqqqqqu. tqqqqqqqqqqqqu. tqqqqqj  x"
3190 GFL$(20)="x O ....... ................. .........  .......... ............... .......O x"
3200 GFL$(21)="x                                                                            x"
3210 GFL$(22)="mqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqj"
3390 GOTO 4450 !Resume main code
3395 !** Graphics Helper Funct. **
3398 !FNSP$: Display sprite
3400 DEF FNSP$(CURSP%,X%,Y%)
3410 PRINT FNAD$(Y%,X%);SP$(CURSP%*2);FNAD$(Y%+1%,X%);SP$((CURSP%*2%)+1%)
3445 FNEND
3446 !FNRD$: Redraw background
3450 DEF FNRD$(X%,Y%)
3470 PRINT FNAD$(Y%,X%);CHR$(GFL%(X%,Y%));CHR$(GFL%(X%+1,Y%));
3480 PRINT FNAD$(Y%+1,X%);CHR$(GFL%(X%,Y%+1));CHR$(GFL%(X%+1,Y%+1));
3490 FNEND
3499 !FNOBJ%: Object detection
3500 DEF FNOBJ%(DIR%,X%,Y%)
3510 IF DIR%=1 AND X%=TLX% AND Y%=TLY% THEN OO%=4%\GOTO 3780 !Left Tun.
3520 IF DIR%=3 AND X%=TRX% AND Y%=TRY% THEN OO%=5%\GOTO 3780 !Right Tun.
3608 !O1%,O2%:Objects in sprite's direction of motion
3615 O1%=GFL%(X%+CX%(DIR%,1%),Y%+CY%(DIR%,1%))
3620 O2%=GFL%(X%+CX%(DIR%,2%),Y%+CY%(DIR%,2%))
3630 IF (O1%=EMPTY% AND O2%=EMPTY%) THEN OO%=0\GOTO 3780 !Empty
3650 IF ((O1%=EMPTY% AND O2%=DOT%) OR (O2%=EMPTY% AND O1%=DOT%)) THEN OO%=1%\GOTO 3780 !Dot
3670 IF ((O1%=EMPTY% AND O2%=PILL%) OR (O2%=EMPTY% AND O1%=PILL%)) THEN OO%=2%\GOTO 3780 !Pill
3750 OO%=6% !Wall
3780 FNOBJ%=OO% \FNEND
3899 !FNDIST(S1%,S2%)  - Calculate distance between two sprites
3900 DEF FNDIST(S1%,S2%) = SQR(ABS(SPY%(S2%)-SPY%(S1%))*2 + ABS(SPX%(S2%)-SPX%(S1%))*2)
3998 !FNGHBX%: Detect inside ghost box
4000 DEF FNGHB%(X%,Y%)
4010 IF (X%>=38 AND X%<=42) AND (Y%>=10 AND Y%<=12) THEN FNGHB%=-1 ELSE FNGHB%=0
4020 FNEND
4029 !FNMOD(x,m) - Calculate modulus x mod m
4030 DEF FNMOD(X,M) = ((X/M)-INT(X/M))*M
4039 !FNDIR%: Return direction of sprite S1 to arbitrary x,y
4040 DEF FNDIR%(S1%,X2%,Y2%)
4050 X1%=SPX%(S1%)\Y1%=SPY%(S1%)
4060 XSGN=SGN(X2%-X1%)\YSGN=SGN(Y2%-Y1%)
4070 IF (XSGN=0 AND YSGN=0) THEN FNDIR%=0\GOTO 4190!Direction 0 (none)
4080 IF (XSGN=-1 AND YSGN=0) THEN FNDIR%=1\GOTO 4190!Direction 1 Left
4090 IF (XSGN=1 AND YSGN=0) THEN FNDIR%=3\GOTO 4190!Direction 3 Right
4100 IF (XSGN=0 AND YSGN=-1) THEN FNDIR%=2\GOTO 4190!Direction 2 Up
4110 IF (XSGN=0 AND YSGN=1) THEN FNDIR%=4\GOTO 4190 !Direction 4 Down
4190 FNEND
4200 DEF FNRLD%(DIR%,OFF%) !FNRLD: Return relative direction of left or right of sp.
4220 XD%=DIR%+OFF%
4230 IF XD%>4 THEN XD%=1 !Wraparound dir.
4240 IF XD%<1 THEN XD%=4
4250 FNRLD%=XD%
4290 FNEND
4300 DEF FNTUN$(S1%,DIR%) !FNTUN$:  Move sprite through tunnel
4310 X$=FNRD$(SPX%(S1%),SPY%(S1%)) !Erase old position
4320 IF NOT DIR%=4 THEN 4340 !Left side tunnel
4330 SPX%(S1%)=TRX%\SPY%(S1%)=TRY%\GOTO 4360
4340 IF NOT DIR%=5 THEN 4360 !Right side tunnel
4350 SPX%(S1%)=TLX%\SPY%(S1%)=TLY%
4360 FNEND
4380 DEF FNSCORE(PTS) !Tally up score and automatically add extra life
4390 SCORE=SCORE+PTS\LSCORE=LSCORE+PTS
4400 IF LSCORE>EXTLF THEN PLAY%=PLAY%+1\LSCORE=0
4405 PRINT FNAD$(0,0);\PRINT USING "SCORE: ###,###,###",SCORE ! Move score display inside function (required by BASIC-PLUS-2)
4407 PRINT FNAD$(0,55);\PRINT USING "LIVES: ###",PLAY%; ! Move player lives display inside function (required by BASIC-PLUS-2)
4410 FNEND
4445 !** MAIN CODE START **
4450 DLY=0\X=TIME(0)
4460 DLY=DLY+1 UNTIL (TIME(0)-X)>=2 !Compute 1 second delay
4470 DLY=DLY/2
4500 ABG%=0% ! Reset abort game flag
4800 OPEN "KB:" FOR INPUT AS FILE 1%
4820 XX$=SYS(CHR$(6%)+CHR$(-7%))! Control-C trap
4880 XX%=SPEC%(7%,0%,1%,2%) !Disable type-ahead
4890 XX%=SPEC%(0%,0%,1%,2%) !Cancel Ctrl-O
4900 XX%=SPEC%(3%,0%,1%,2%) !Disable echo
4960 FIELD #1%, 128% AS B$
4970 LSET B$="" !Flush buffer
9991 !** MAIN GAME LOOP AND NEW GAME ENTRY **
10000 CM$=SYS(CHR$(7)) ! Check if game being resumed
10010 IF LEFT(CM$,6)<>"HACMAN" THEN SCORE=0\LSCORE=0\PLAY%=MAXLF%\LEV%=0%\GOTO 10070! No chained data, new game
10030 SCORE=CVT$F(MID(CM$,7,LEN(CVTF$(0))))\LEV%=CVT$%(MID(CM$,7+LEN(CVTF$(0)),2))\PLAY%=CVT$%(MID(CM$,9+LEN(CVTF$(0)),2))\LSCORE=SC
ORE
10070 ON ERROR GOTO 19900 !Set error handling for main game loop
10095 !** NEW LEVEL ENTRY POINT **
10100 DOTS%=0% \ IF LEV%<255 THEN LEV%=LEV%+1 !Nod to original level limit
10105 PLTM=0 !Pill timer not active
10110 IF LEV%<11 THEN MAXPLT=15-LEV% ELSE MAXPLT=1
10115 IF LEV%<=10 THEN VEL%=INT((10-LEV%)/5)+1 ELSE VEL%=1 !"Speed" of ghost, lower numbers are faster
10119 !Initialize background maze collision detect array
10120 MAXDOT%=0%\FOR IY%=2% TO 23%\FOR IX%=1% TO 79%
10130 GFL%(IX%,IY%)=ASCII(MID(GFL$(IY%-1%),IX%,1%))
10140 IF (GFL%(IX%,IY%)=DOT% OR GFL%(IX%,IY%)=PILL%) THEN MAXDOT%=MAXDOT%+1
10141 !Count up total dots including 4 power pills
10150 NEXT IX%\NEXT IY%
10200 !Draw maze and score display
10210 PRINT FNCL$+FNLD$ ! Clear screen and alt character set
10220 GOSUB 19010\GOSUB 19050 ! Print score and level
10250 PRINT FNHM$;! Home cursor
10260 PRINT\FOR I%=1% TO 22%\PRINT GFL$(I%)\NEXT I%
10300 !Place bonus sprite and indicate according to level
10310 FRTSP% = FNMOD(LEV%,3)+FRUIT%
10320 XX$=FNSP$(FRTSP%,STX%(FRUIT%),STY%(FRUIT%))
10330 SPX%(FRUIT%)=STX%(FRUIT%)\SPY%(FRUIT%)=STY%(FRUIT%)
10340 SSTAT%(FRUIT%)=-1 !Reset fruit status to uneaten
10400 !*** Resume Play Entry Point ***
10405 !Reset player sprite position, direction and location
10410 PLPOS%=0\OLDPOS%=PLPOS%!Player sprite is still (not moving)
10420 SPX%(0%)=STX%(0%)\SPY%(0%)=STY%(0)
10430 XX$=FNSP$(PLPOS%,SPX%(0%),SPY%(0%)) !Display sprite
10440 GOSUB 19100 !Display player lives
10495 !Reset ghost sprites position and status
10600 FOR I%=GHOST% TO MAXGH%
10610 SPX%(I%)=STX%(I%)\SPY%(I%)=STY%(I%)\SSTAT%(I%)=0%\SDR%(I%)=SSDR%(I%)\SV%(I%)=VEL%
10620 XX$=FNSP$(I%,SPX%(I%),SPY%(I%)) !Display sprite
10630 NEXT I%
10995 !** INPUT LOOP **
11000 XX%=SPEC%(4,0,1,2%) !Set ODT Mode
11010 GET #1%,RECORD 8192%
11020 I$=LEFT(B$,RECOUNT)
11030 IF I$<>ESKB$ THEN GOTO 11300 !If it's not an escape sequence, we are done getting chars.
11040 XX%=SPEC%(4%,0%,1%,2%)!Set ODT mode again
11050 GET #1%, RECORD 8192% !And get the rest of the sequence
11060 I$=I$+LEFT(B$,RECOUNT)
11062 LSET B$=""
11070 I2%=INSTR(1%,ARKY$,I$)/3
11080 IF I2%<>0% THEN ON I2% GOSUB 11100,11150,11200,11250
11095 GOTO 12000
11100 PLPOS%=1\RETURN !*Left direction
11150 PLPOS%=2\RETURN !*Up direction
11200 PLPOS%=3\RETURN !*Right direction
11250 PLPOS%=4\RETURN !*Down direction
11295 !**Non-arrow key detection
11300 I1%=INSTR(1%,CMKY$,CVT$$(I$,32%))!Convert input to uppercase before checking
11305 LSET B$=""
11310 IF I1%<>0 THEN ON I1% GOTO 11350,11450
11330 GOTO 12000 !No valid input, resume play
11350 ABG%=-1%\GOTO 23000 !* Q-End Game
11450 PLPOS%=0 !* Space bar (hold position)
11998 !** GHOST AI
12000 IF PLTM = 0 THEN 12200  !If pill timer not activated, skip
12020 IF (TIME(0)-PLTM) < MAXPLT THEN 12200 !Pill timer not expired yet
12021 !Pill mode over, redraw normal ghosts
12030 FOR I%=GHOST% TO MAXGH%\SSTAT%(I%) = 0\XX$=FNSP$(I%,SPX%(I%),SPY%(I%))\NEXT I%
12040 PLTM=0\BONTM=0\BONUS=0 !Reset pill timer and bonus count
12198 !Ghost movement routine
12200 FOR I%=GHOST% TO MAXGH%
12201 !Apply a delay count before moving ghost;  skip moving if delay timer not expired
12210 SV%(I%)=SV%(I%)-1\IF SV%(I%)>0 THEN 13480 ELSE SV%(I%)=VEL%
12220 IF NOT FNGHB%(SPX%(I%),SPY%(I%)) THEN 12300
12229 !If in ghost box, move ghost up through door
12230 IF SSTAT%(I%) THEN 12330 !If in box and eatable, do not move!
12240 SDR%(I%)=2\GOTO 13350 !End ghost box exit AI
12298 !Select preferred target for each ghost
12300 IF NOT SSTAT%(I%) THEN 12330 !Skip if not eatable
12301 !Same strategy for eatable mode for all ghosts
12302 !Preferred: Move away from Hac-Man
12310 GP%=REV%(FNDIR%(I%,SPX%(0),SPY%(0)))
12320 GOTO 13200
12323 !Per-ghost target calculations
12330 ON (I%-GHOST%)+1 GOSUB 12400,12500,12600,12900
12350 GP%=FNDIR%(I%,PX%,PY%)
12390 GOTO 13200
12398 !Ink: Head directly for Hac-Man
12400 PX%=SPX%(0)\PY%=SPY%(0%)\RETURN
12498 !Blink:  Head in front of Hac-Man
12500 PX%=SPX%(0)+DX%(SDR%(0))\PY%=SPY%(0)+DY%(SDR%(0))\RETURN
12598 !Pink: Head behind Hac-Man
12600 PX%=SPX%(0)-DX%(SDR%(0))\PY%=SPY%(0)-DY%(SDR%(0))\RETURN
12895 !Sal: Go for Hac-Man if far, hide when close
12900 SD=FNDIST(I%,0)\IF SD>8 THEN PX%=SPX%(0)\PY%=SPY%(0)\RETURN
12920 PX%=70%\PY%=9\RETURN !Sal's corner is in upper right
13195 !Select direction:  Priority is Preferred (calculated), straight-ahead, sprite left, sprite right
13200 DIM G%(3)
13210 G%(0)=GP%\G%(1)=SDR%(I%)\G%(2)=FNRLD%(SDR%(I%),-1)\G%(3)=FNRLD%(SDR%(I%),1)
13220 IX%=0
13230 GD%=FNOBJ%(G%(IX%),SPX%(I%),SPY%(I%)) !Check each direction if clear
13240 IF GD%=4 OR GD%=5 THEN XX$=FNTUN$(I%,GD%)\GOTO 13460
13250 IF GD%<=2 THEN SDR%(I%)=G%(IX%)\GOTO 13350
13260 IF IX%<=2 THEN IX%=IX%+1\GOTO 13230
13270 PRINT FNNC$;"NO DIRECTION FOUND-G,GD,IX,I";\MAT PRINT G%\PRINT IX%;I%\stop
13350 XX$=FNRD$(SPX%(I%),SPY%(I%))
13440 SPX%(I%)=SPX%(I%)+DX%(SDR%(I%))\SPY%(I%)=SPY%(I%)+DY%(SDR%(I%))
13450 IF NOT SSTAT%(I%) THEN IS%=I% ELSE IS%=9 ! Draw ghost sprite or eatable sprite
13460 XX$=FNSP$(IS%,SPX%(I%),SPY%(I%))
13461 !Ghost/fruit collision detection
13465 DF=FNDIST(I%,FRUIT%) !Check if fruit overdrawn
13467 IF DF=0 AND SSTAT%(FRUIT%) THEN XX$=FNSP$(FRTSP%,SPX%(FRUIT%),SPY%(FRUIT%))
13480 NEXT I% !End AI routine
13500 DEAD%=0 !Player dead flag cleared
13510 FOR I%=GHOST% TO MAXGH%
13520 IF FNDIST(0%,I%)<>0 THEN 13650 !Skip this ghost, if not close
13530 XX$=FNRD$(SPX%(I%),SPY%(I%)) !Reset this ghost
13540 SPX%(I%)=STX%(I%)\SPY%(I%)= STY%(I%)\ SDR%(I%)=SSDR%(I%)\XX$=FNSP$(I%,SPX%(I%),SPY%(I%))
13550 IF NOT SSTAT%(I%) THEN DEAD%=-1\ GOTO 13650 !Ghost is normal, player is toast
13560 IF BONUS=0 THEN BONUS=200 ELSE BONUS=BONUS*2 !Multiply bonus
13565 XX=FNSCORE(BONUS)\BONTM=TIME(0%)\GOSUB 19200
13570 SSTAT%(I%)=0
13650 NEXT I%
13660 IF NOT DEAD% THEN 14370  !Continue if death flag not set
13670 PRINT CHR$(0)+CHR$(0)+CHR$(0)+CHR$(7)+CHR$(7)+CHR$(7) !Bell
13680 PLAY%=PLAY%-1\IF PLAY%<1 THEN 23000 !End game
13690 XX$=FNSP$(SPDEAD%,SPX%(0),SPY%(0)) !Draw death sprite
13700 FOR I%=GHOST% TO MAXGH%\XX$=FNRD$(SPX%(I%),SPY%(I%))\NEXT I%
13710 XX$=FNRD$(SPX%(0),SPY%(0)) !Erase HacMan
13790 GOTO 10400 !Resume play
14369 !Check for fruit-skip if player not moving, fruit eaten or fruit not reached
14370 IF PLPOS%=0% OR (NOT SSTAT%(FRUIT%)) OR FNDIST(0,FRUIT%)<>0 THEN 14450
14400 IF LEV%=1 THEN BONUS=FRTSC ELSE BONUS=FRTSC+(LEV%*50)
14405 SSTAT%(FRUIT%)=0% !Selected fruit now eaten
14410 XX=FNSCORE(BONUS)\GOSUB 19200\BONUS=0\BONTM=TIME(0%)
14449 !Blank bonus display on timeout
14450 IF (TIME(0%)-BONTM)<5 THEN 14500 !Display bonus for 5 sec.
14460 BONTM=0\PRINT FNAD$(0,30);SPACE$(16) !Reset bonus
14498 !Obj. detect
14500 IF PLPOS%=0 THEN GOTO 15500 !Skip player motion calc if player does not move
14510 O%=FNOBJ%(PLPOS%,SPX%(0),SPY%(0)) !What's in player's direction?
14620 ON O%+1 GOSUB 15000,15050,15100,15250,15400,15400,15250
14630 GOTO 15500 !Continue
14998 !Object case 0: No object--continue moving
15000 XX$=FNRD$(SPX%(0),SPY%(0)) !Update background
15010 SPX%(0)=SPX%(0)+DX%(PLPOS%) \ SPY%(0)=SPY%(0)+DY%(PLPOS%)
15020 XX$=FNSP$(PLPOS%,SPX%(0),SPY%(0))
15040 RETURN
15049 !Object case 1: Dot--erase from object array and add points
15050 GFL%(SPX%(0)+CX%(PLPOS%,1%),SPY%(0)+CY%(PLPOS%,1%))=EMPTY%
15060 GFL%(SPX%(0)+CX%(PLPOS%,2%),SPY%(0)+CY%(PLPOS%,2%))=EMPTY%
15070 XX$=FNRD$(SPX%(0),SPY%(0)) !Update background
15075 SPX%(0)=SPX%(0)+DX%(PLPOS%)\SPY%(0)=SPY%(0)+DY%(PLPOS%)
15080 XX$=FNSP$(PLPOS%,SPX%(0),SPY%(0))
15085 DOTS%=DOTS%+1\XX=FNSCORE(DOTSC)
15090 RETURN
15095 !Object case 2: Pill--erase from object array, add points and set eatable flag for each ghost
15100 GFL%(SPX%(0)+CX%(PLPOS%,1%),SPY%(0)+CY%(PLPOS%,1%)) = EMPTY%
15110 GFL%(SPX%(0)+CX%(PLPOS%,2%),SPY%(0)+CY%(PLPOS%,2%)) = EMPTY%
15120 XX$=FNRD$(SPX%(0),SPY%(0)) !Update sprite position
15130 SPX%(0)=SPX%(0)+DX%(PLPOS%)\SPY%(0)=SPY%(0)+DY%(PLPOS%)
15150 XX$=FNSP$(PLPOS%,SPX%(0),SPY%(0))
15160 DOTS%=DOTS%+1\XX=FNSCORE(DOTSC)! Update score
15169 !Power pill eaten--turn ghosts and reverse their direction
15170 FOR I%=GHOST% TO MAXGH%\XX$=FNSP$(PGHOS%,SPX%(I%),SPY%(I%))\SSTAT%(I%)=-1%\SDR%(I%)=REV%(SDR%(I%))\NEXT I%
15180 PLTM=TIME(0%)!Start pill timer
15240 RETURN
15249 !Object Case 3 and 6: Wall and ghost door detect
15250 PLPOS%=0
15280 XX$=FNSP$(PLPOS%,SPX%(0),SPY%(0))
15390 RETURN
15398 !Object case 4 and 5: Left or right tunnel--Reset position
15400 XX$=FNTUN$(0,O%) !Erase old sprite
15410 RETURN
15498 !Save position;  update sprite
15500 XX$=FNSP$(PLPOS%,SPX%(0),SPY%(0))
15550 OLDPOS%=PLPOS%
15600 FOR XX=0 TO INT(DLY*0.75)\NEXT XX !Delay loop
15750 IF (DOTS%<MAXDOT%) THEN GOTO 11000 !Resume top of game loop *
15760 IF LEV%<2 OR FNMOD(LEV%,2)<>0 THEN GOTO 10100 ! No halftime show
15800 GM$="HACMAN"+CVTF$(SCORE)+CVT%$(LEV%)+CVT%$(PLAY%)
15810 XX$=SYS(CHR$(8%)+GM$) !Pass score, level, lives to core common
15820 IF NOT DEBUG% THEN CHAIN "HACMAN" LINE 8000 ! Chain halftime show
15830 GOTO 10000
19005 !** Gameloop Helper Code **
19009 !* Display score at top of screen
19010 PRINT FNAD$(0,0);\PRINT USING "SCORE: ###,###,###",SCORE\RETURN
19049 !* Display level at top right of screen
19050 PRINT FNAD$(0,67);\PRINT USING "LEVEL: ###",LEV%;\RETURN
19099 !* Display lives at botton
19100 PRINT FNAD$(0,55);\PRINT USING "LIVES: ###",PLAY%;\RETURN
19199 !* Display special score
19200 PRINT FNAD$(0,30);"*** ";BONUS;" ***";\RETURN
19895 !** Game loop error handling
19900 IF ERR=13 AND (ERL=11010 OR ERL=11050) THEN RESUME 12000 !No KB input, resume loop
19910 IF ERR=28 THEN ABG%=-1%\GOTO 23000 !Control-C trap: End game
19950 PRINT FNNC$ !Reset normal characters
19960 XX%=SPEC%(2%,0%,1%,2%)!Re-enable echo
19970 ON ERROR GOTO 0!Enable RSTS error handling
19980 STOP
22998 !Game over code--chain back to front end
22999 !If game aborted do not pass score data back to front end
23000 IF ABG% THEN GM$="" ELSE GM$="HACMAN"+CVTF$(SCORE)+CVT%$(LEV%)+CVT%$(PLAY%)
23040 XX$=SYS(CHR$(8%)+GM$)
23050 IF NOT DEBUG% THEN CHAIN "HACMAN" LINE 8500
23060 PRINT FNNC$;FNCL$ ! Reset ANSI normal character set and clear screen
23070 XX%=SPEC%(2%,0%,1%,2%)!Re-enable echo
23080 CLOSE 1%
31990 IF DEBUG% THEN CLOSE 3%
32000 END
 
 
