# Apple II Croosword Solver

Here is a program for solving crossword puzzles, working in the 128 kB (only) of an Apple IIe, IIc, or IIGS of course.
The challenge was to find the "hole words" among the 386 264 French words allowed in Scrabble, from 2 to 15 letters.

Of course, the data (words and indexes) do not fit on a floppy disk, but on a hard disk (physical or virtual) and occupy about 10 MB.

## Use
This archive contains a ProDOS disk image (cw.po) to be used it your favourite Apple II emulator or your Apple II.
* Start your Apple II with the "cw.po" disk.
* Launch the program with "brun cw", or "-cw"
* Type the capital letters, or ? for unknown letters. Enter to validate. Escape or ctrl-c to exit.

## Performance
On an Apple II or an emulator set at normal speed, the processing of each significant letter takes about 17 seconds.
It is certainly possible to improve the performance, for example by using uncompressed indexes (I used RLE compression, but it may not be a good idea, to be explored).

## Credits
The algorithm is the one used in the French software "Easy Puss", for those who remember this database software for Apple II published in the 80's. It is applied to letters and their positions in this case.
I found the list of words here: http://www.3zsoftware.com/fr/listes.php
(ods6.txt)
ProDOS : "ProDOS 8 Technical Reference Manual", "Beneath Apple ProDOS"

## Requirements to compile and run

Here is my configuration:

* Visual Studio Code with 2 extensions :

-> [Merlin32 : 6502 code hightliting](marketplace.visualstudio.com/items?itemName=olivier-guinart.merlin32)

-> [Code-runner :  running batch file with right-clic.](marketplace.visualstudio.com/items?itemName=formulahendry.code-runner)

* [Merlin32 cross compiler](brutaldeluxe.fr/products/crossdevtools/merlin)

* [Applewin : Apple IIe emulator](github.com/AppleWin/AppleWin)

* [Applecommander ; disk image utility](applecommander.sourceforge.net)

* [Ciderpress ; disk image utility](a2ciderpress.com)

Note :
DoMerlin.bat puts it all together. If you want to compile yourself, you will have to adapt the path to the Merlin32 directory, to Applewin and to Applecommander in DoMerlin.bat file.

DoMerlin.bat is to be placed in project directory.
It compiles source (*.s) with Merlin32, copy 6502 binary to a disk image (containg ProDOS), and launch Applewin with this disk in S7,D1.

