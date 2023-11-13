# rstse-basic-hacman
Clone of Pac-Man for PDP-11 RSTS/E BASIC.  Recreated in 2012 from an original project in 1981.  Requires a PDP-11 or emulated system running RSTS/E and a true VT-100 terminal or emulation.

For more information, see my series of blog posts, "Retrochallenge: Hac-Man" at https://davidcmoisan.wordpress.com/2012/07/05/hac-man-retrochallenge/ 

Two files are included: HACMAN.BAS and HACMAG.BAS.  Both of these are required to run the game.

HACMAN.BAS is the main program and front-end for the game and must be run first;  HACMAG.BAS is the main game engine itself.
Here are the "build instructions".  HACMAN has been tested on emulated RSTS/E BASIC, RSTS version 7, and RSTS version 10.1 with the Windows terminal emulator, Tera Term.
The instructions should not vary, but if you are running this on real hardware, this is best done from a Windows or Linux system with a terminal program.    

1. Load the first file, HACMAN.BAS, into a text editor on your emulated host, or on the same machine that your terminal program is running on.
2. Log into RSTS BASIC. This can be done in a system (1,*) or non-system account.
3. In BASIC, type NEW HACMAN.
4. Copy the text of the program from your text editor and paste it into your terminal.
5. Hit return several times to be sure the program entered.
6. Type LIST.  If you get a (long) program listing, then it probably transferred successfully.
7. Type SAVE HACMAN.
8. Type NEW HACMAG. 
9. Load the second file, HACMAG.BAS into a text editor and then copy it and paste it into the terminal as in step 1.
10. Type LIST.  You should get a long listing--this is the main logic as described in my blog.
11. Type SAVE HACMAG.
12. Type DIR.  You should see two files, HACMAN.BAS and HACMAG.BAS.
13. I strongly recommend compiling both programs for speed and ease of use;  the two programs chain each other during play, and they may not work if not compiled.
14. Type OLD HACMAN.
15. Type COMPILE.
16. Type OLD HACMAG.
17. Type COMPILE.
18. The program is ready to play!

To run the game, type RUN HACMAN.  The game will start and display the title.  From here on, the game is played with the arrow keys and is very much like playing an old DOS game.
The game is a clone of the famous, iconic, and historic, maze game that came from Japan at around the same time I wrote the original clone as a high-school student in 1981.

I wrote this recreation of my school computer exploits as a homage, and a period piece to those days.  With vintage computing being as popular as it is now, I'm proud to present this 
piece of history.  Have fun!

--David Moisan
