************************
*   crossword solver   * 
************************
* Purpose : make search in a 386 264 french words list.
* Uses run length encoded bitmap index files.
*
* History :
* version 1.0 : uses tempo files 
* version 1.1 : get rid of tempo files, works in memory mostly.
* version 1.2 : add a progress bar
*
* IMPORTANT : 
* The creation of index files is done with the Delphi program here :
* https://github.com/bruno185/Make-index-for-Apple-II-crossword-solver
*
* MLI calls (ProDOS)
MLI             equ $BF00
create          equ $C0
destroy         equ $C1
online          equ $C5
getprefix       equ $c7
setprefix       equ $c6
open            equ $C8
close           equ $CC
read            equ $CA
write           equ $CB
setmark         equ $ce
geteof          equ $d1 
quit            equ $65
*
* ProDOS
GETBUFR         equ $bef5
FREEBUFR        equ $BEF8 
devnum          equ $BF30   ; last used device here, format : DSSS0000 
RSHIMEM         equ $BEFB
*
* ROM routines
home            equ $FC58
text            equ $FB2F
;cout            equ $FDF0
cout            equ $FDED
vtab            equ $FC22
getln           equ $FD6A
getlnz          equ $FD67       ; = return + getln
getln1          equ $FD6F       ; = getln without prompt 
bascalc         equ $FBC1
crout           equ $FD8E     ; print carriage return 
clreop          equ $FC42     ; clear from cursor to end of page
clreol          equ $FC9C     ; clear from cursor to end of line
xtohex          equ $F944
rdkey           equ $FD0C     ; wait for keypress
printhex        equ $FDDA
AUXMOV          equ $C311
OUTPORT         equ $FE95
*
* ROM switches
RAMWRTOFF       equ $C004       ; write to main
RAMWRTON        equ $C005       ; write to aux
RAMRDON         equ $C003       ; read aux 
RAMRDOFF        equ $C002       ; read main
ALTCHARSET0FF   equ $C00E 
ALTCHARSET0N    equ $C00F
kbd             equ $C000
kbdstrb         equ $C010
col80off        equ $C00C
col80on         equ $C00D
80col           equ $C01F 	 
*
* page 0
cv              equ $25
ch              equ $24 
basl            equ $28
wndlft          equ $20
wndwdth         equ $21
wndtop          equ $22         ; Top Margin (0 - 23, 0 is default, 20 in graphics mode)
wndbtm          equ $23 
prompt          equ $33
*
ourch           equ $57B      ; Cursor's column position minus 1 (HTAB's place) in 80-column mode
ourcv           equ $5FB      ; 80 col vertical pos
*
****** FP routines ******
float   equ $E2F2       ; Converts SIGNED integer in A/Y (high/lo) into FAC 
PRNTFAC equ $ED2E       ; Prints number in FAC (in decimal format). FAC is destroyed
FIN     equ $EC4A       ; FAC = expression pointee par TXTPTR
FNEG    equ $EED0       ; FAC = - FAC
FABS    equ $EBAF       ; FAC = ABS(FAC)
F2INT16 equ $E752       ; FAC to 16 bits int in A/Y and $50/51 (low/high)
FADD    equ $E7BE       ; FAC = FAC + ARG 
FSUBT   equ $E7AA       ; FAC = FAC - ARG
FMULT   equ $E97F       ; Move the number pointed by Y,A into ARG and fall into FMULTT 
FMULTT  equ $E982       ; FAC = FAC x ARG
FDIVT   equ $EA69       ; FAC = FAC / ARG
RND     equ $EFAE       ; FAC = random number
FOUT    equ $ED34       ; Create a string at the start of the stack ($100−$110)
MOVAF   equ $EB63       ; Move FAC into ARG. On exit A=FACEXP and Z is set
CONINT  equ $E6FB       ; Convert FAC into a single byte number in X and FACLO
YTOFAC  equ $E301       ; Float y 
MOVMF   equ $EB2B       ; Routine to pack FP number. Address of destination must be in Y
                        ; (high) and X (low). Result is packed from FAC
QUINT   equ $EBF2       ; convert fac to 16bit INT at $A0 and $A1
STROUT  equ $DB3A       ; 
LINPRT  equ $ED24       ; Converts the unsigned hexadecimal number in X (low) and A (high) into a decimal number and displays it.
*
***********************    my equ  ***********************
bitmap1  equ $2000      ; $2000 -> $4FFF (bitmap index 1) => aux mem.
bitmap2  equ $5000      ; $5000 -> $7FFF (bitmap index 2) => aux mem.
indexrl  equ $2000      ; loads RLE index here  => main mem.
ptr1     equ $06        ;
ptr2     equ $08
reclength       equ $10 ; length of record in words file
pbline  equ $03         ; # of text line for progressbar
pbchar  equ #'>'        ; char for progressbar
pbora   equ #$80        ; char bit 7 for progressbar

*
********************  memory org.  ***********************
* program : $1000 (main AND aux memory, using AUXMOV)
* RLE index file loading address : $2000 (main memory) => < $8200 (for biggest RLE file )
* bitmap1 (for uncompressed RLE index file): $2000 -> $4FFF (aux memory)  
* bitmap2 (for uncompressed RLE index file): $5000 -> $7FFF (aux memory)
* buffer for OPEN MLI call (1024 bytes), RLE files : $8400
* buffer for OPEN MLI call (1024 bytes), WORDS files : $8800
*
********************  main program  **********************
        MX %11
        put mac.s
        org $1000

init    equ *
        cld
        ;lda #$3
        ;jsr OUTPORT    ; 80 col.
        ; lda #$15      ; 40 cols, Character that turns off video firmware (ProDOS technical reference)
        jsr $C300       ; 80 col. (http://www.deater.net/weave/vmwprod/demos/sizecoding.html)
        jsr text        ; clear screen
        jsr home        ; cursor to upper left corner
        jsr ramout      ; disconnect /RAM disk
        printc titlelib
        cr
        lda #$00
        sta $BF94
        closef #$00     ; close all files

* set prefix
        jsr doprefix    ; set prefix
        prnstr path     ; print it
        cr

        prnstr patternlib
        jsr mygetln     ; let user type pattern
        jsr testpat     ; if no letter in partter, googpat var 
        lda quitflag    ; if ctrl-c or escape then quitflag > 0
        bne exit2       
        lda pattern     ; get pattern length
        cmp #$02        ; pattern length msut be >= 2
        bpl okpat
        cr
        prnstr kopatlib ; wrong pattern, message and loop
        jsr dowait      ; wait for a key pressed
        jmp init        ; goto beginning
exit2   rts             ; end of program

okpat   cr
        cr
*
********************  init  **********************
        jsr copymaintoaux  ; copy program in aux mem at same address, using AUXMOV

        lda #$00        ; init total counter (sum of counters for 4 parts)
        sta totalcnt    
        sta totalcnt+1
        sta totalcnt+2
        
        sta wordscnt     ; init word counter to 0
        sta wordscnt+1
        sta wordscnt+2

        sta col         ; horiz. position 

        sta pbpos       ; init progressbar in position 0

        lda #$01        ; start with part 1
        sta part

        lda #4          ; set top margin to 4 
        sta wndtop

********************  MAIN LOOP  **********************
main    
        closef #$00     ; close all files
        jsr FREEBUFR    ; free all buffers 
        jsr bigloop     ; main program loop : porcess all letters for one part
        ;jsr progressbar
        jsr bigdisplay  ; prints found words 
        jsr updatetotcnt ; update total found
        jsr progressbar

        inc part        ; next part (on 4)
        lda part 
        cmp #$05        ; done ?
        bne main        ; no : loop

        jsr showres     ; show final result (count)

eop     jsr dowait      ; wait for a pressed key 
        closef #$00     ; close all files 
        jsr FREEBUFR    ; free all buffers
        jmp init        
*
******************** main program end ********************
progressbar
        lda #pbline     ; get line # for progressbar
        jsr bascalc     ; get base address 
        lda pbpos       ; get last h positino
        clc             ; add it to pointer
        adc basl
        sta basl
        lda #$00
        adc basl+1
        sta basl+1
        lda pbchar      ; get char to display in progressbar
        ora pbora       ; ora parameter char 
        ldy #$00        ; init loop counter
        sta $C000       ; 80store on
ploop
        sta RAMWRTON    ; write char in aux
        sta (basl),y 
        sta RAMWRTOFF
        sta (basl),y    ; write char in aux
        
        inc pbpos       ; update h position
        iny             ; inc counter
        cpy #10         ; test end of loop
        beq pbexit      ; end : exit
nibas   jmp ploop       ; go on

pbexit  sta $C001       ; 80store off
        rts

*************************************
bigloop lda #$01
        sta pos         ; position in pattern = 1
        clc
        jsr fillmem     ; fill bitmap1 ($2000 in aux memory) with $ff

bigll   
        lda noletter    ; letter in pattern ?
        bne dolong      ; no : jump to length index process
                        ; yes : search (full process)
        ldx pos         ; x =  position in pattern
        dex             ; adjust (x must start from 0, pos start from 1)
        lda pattern+1,x ; get char from pattern
        cmp #'A'        ; char between A and  Z ? 
        bcc bloopnext   ; no : next char in pattern
        cmp #'Z'+1
        bcs bloopnext
        sta letter      ; yes : save char in letter var

        jsr interpret   ; set filename based on letter, position and part

        jsr dofile      ; load RLE file in main, decode in aux ($5000 area)
                        ; AND $2000 area and $5000 area, result in $2000 area (in aux)
dolong  jsr dowlen      ; set RLE index file name for length
        jsr dofile      ; process RLE inde
bloopnext
        inc pos         ; next char in pattern
        ldx pos
        dex             
        cpx pattern     ; end of pattern (1st char = length)
        bne bigll       ; no : loop
        rts
* end bigloop
*
dowlen                  ; Add criterion of word length by load Lx RLE index file 
                        ; x=length of words.
                        ; prepare file name
        lda #$6
        sta fname       ; file name is 6 char long
        lda #'L'        ; L folder
        sta fname+1
        lda #'/'
        sta fname+2
        lda #'L'        ; L is first char of filename
        sta fname+3        
        ldx pattern     ; pattern string length = length of words to find 
        lda tohex,x 
        sta fname+4
        lda #'P'        ; part suffix
        sta fname+5
        lda #'0'
        clc
        adc part
        sta fname+6
        rts
*
* show result of count
showres
        lda totalcnt    ; copy totalcounter var
        sta counter     ; to counter var
        lda totalcnt+1
        sta counter+1       
        lda totalcnt+2
        sta counter+2
        cr
        prnstr patlib   ; recall pattern
        prnstr pattern
        cr
        prnstr totallib ; print lib
        jsr print24bits ; print number of found words   
        rts             ; 
*
updatetotcnt            ; add counter to totalcnt (3 bytes integers)
        clc
        lda counter
        adc totalcnt
        sta totalcnt
        lda counter+1
        adc totalcnt+1
        sta totalcnt+1
        lda counter+2
        adc totalcnt+2
        sta totalcnt+2
        rts
*
copymaintoaux           ; copy program to AUX memory
        lda #>init
        sta $3d         ; source high
        sta $43         ; dest high
        lda #<init      
        sta $3c         ; source low
        sta $42         ; dest low
        lda #>prgend    ; source end low
        sta $3f 
        lda #<prgend    ; source end high
        sta $3e
        sec             ; main to aux
        jsr AUXMOV      ; move
        rts
*
copyindextomain         ; copyfrom AUX to MAIN , same address, length=index bitmap (12071 bytes)
        lda #>bitmap1
        sta $3d         ; source high
        sta $43         ; dest high
        lda #<bitmap1      
        sta $3c         ; source low
        sta $42         ; dest low 
        lda $3c 
        clc
        adc #$27        ; add length to source  (+12071 bytes)
        sta $3e
        lda $3d
        adc #$2f 
        sta $3f
        clc
        jsr AUXMOV      ; move
        rts

*
dofile
* process a RLE file : 
* - load it
* - decode to aux mem
* - AND bitmap1 and bitmap2 memory areas
*
* open RLE file
        jsr setopenbuffer       ; get free mem from ProDOS for OPEN call (mli)
        jsr MLI                 ; OPEN file 
        dfb open
        da  c8_parms
        bcc ok1
        jmp ko
ok1     
* get eof (to get file size)
        lda ref
        sta refd1
        jsr MLI                 ; get file length (set file length for next read MLI call)
        dfb geteof
        da d1_param
        bcc eofok
        jmp ko
eofok        
* read RLE index 
        jsr readindex   ; prepare loading of index file (set ID, req. length, etc.)
        jsr MLI         ; load file in main memory
        dfb read
        da  ca_parms
        bcc okread
        jmp ko
okread  
        lda ref       ; close RLE file
        sta cc_parms+1
        jsr MLI
        dfb close
        da cc_parms
        bcc okclose
        jmp ko
okclose                 ; decode RLE index
        sec 
        jsr decode      ; decode RLE file to bitmap2/bitmap2+1 in aux 
        jsr doand       ; AND $2000 and $5000 areas 
        rts
* end of dofile

setopenbuffer           ; set buffer to $8400 for OPEN mli call
        lda #$00
        sta fbuff
        lda #$84
        sta fbuff+1
        rts

* count bit set to 1 in index
countbit
        lda #>bitmap1   ; set pointer to $2000 area
        sta ptr1+1
        lda #<bitmap1
        sta ptr1
        
        lda #$00        ; init counter
        sta counter
        sta counter+1
        sta counter+2
loopcount
        ldy #$00
        sta RAMRDON
        lda (ptr1),y    ; get byte to read
        sta RAMRDOFF
        beq updateptr   ; byte = $00 : loop
        ldx #$08        ; 8 bits to check
shift   lsr
        bcc nocarry
        iny             ; y counts bits set to 1
nocarry dex
        bne shift       ; loop 8 times

        tya             ; number of bits in A
        beq updateptr   ; no bits to count
        clc             ; add bits to result (counter)
        adc counter
        sta counter
        lda #$00
        adc counter+1
        sta counter+1
        lda #$00
        adc counter+2 
        sta counter+2       
updateptr    
        inc ptr1        ; next byte to read
        bne noincp1
        inc ptr1+1
noincp1
        lda ptr1        ; 12071 bytes (length of index) to scan + 1 (because of inc) = $2F28
        cmp #$28        ; from $2000 to $2000+$2F28 = $4F28 = 12071  + 1
        bne loopcount
        lda ptr1+1
        cmp #$2F + #$20
        bne loopcount
        rts

******************* AND *******************
doand                   ; AND bitmap1 and bitmap2 memory areas (in aux)
        lda #<bitmap1   ; set bitamp1 address in ptr1 
        sta ptr1
        lda #>bitmap1
        sta ptr1+1 
        lda #<bitmap2   ; set bitamp2 address in ptr2 
        sta ptr2
        lda #>bitmap2
        sta ptr2+1  

        ldy #$00
        sta RAMRDON
        sta RAMWRTON
andloop
        lda (ptr1),y    ; get byte from 1st area (bitmap1)
        and (ptr2),y    ; and bye from 2nd area (bitmap2)
        sta (ptr1),y    ; save result in 1st area (bitmap1)

        inc ptr1        ; update pointers
        bne ni
        inc ptr1+1
ni      inc ptr2
        bne ni2
        inc ptr2+1
ni2     lda ptr1+1
        ;cmp #>bitmap2   ; end of area ?
        cmp #$4f        ; area is $2F27 long (and strats at $2000)
        bne andloop
        lda ptr1
        cmp #$28
        bne andloop
        sta RAMRDOFF
        sta RAMWRTOFF
        rts
* NB : "area" is $3000 byte long. it is a little more 
* than actual index size (12071 bytes = $2F27 bytes)

************** readindex **************
readindex               ; read RLE index file into RAM
        lda ref         ; get file ref id
        sta refread     ; set ref id for read mli call

        lda #<indexrl   ; set buffer address
        sta rdbuffa
        lda #>indexrl
        sta rdbuffa+1       

        lda filelength  ; set requested length (= length obtained by get_eof)
        sta rreq
        lda filelength+1
        sta rreq+1
        rts

****************** decode ****************
*                  input :               *
* if carry clear : destination = bitmap1 *
* if carry set   : destination = bitmap2 *
******************************************
decode
        bcs bmp2 

        lda #<bitmap1   ; set bitamp1 address in ptr2 (destination)
        sta ptr2
        lda #>bitmap1
        sta ptr2+1
        jmp bmp1
bmp2
        lda #<bitmap2   ; set bitamp2 address in ptr2 (destination)
        sta ptr2
        lda #>bitmap2
        sta ptr2+1
bmp1 
        lda filelength  ; init counter with filelength
        sta counter
        lda filelength+1
        sta counter+1
        ;jsr countbyte   ; count bytes 

        lda filelength+1 ; counter = filelength / 2
        lsr
        sta counter+1
        lda filelength
        ror 
        sta counter
        
        lda #<indexrl   ; set indexrl address in ptr1 (source)
        sta ptr1
        lda #>indexrl
        sta ptr1+1     
decloop
        ldy #$00        ; get a pair of bytes (length and value)
        lda (ptr1),y    ; get length (= number of bytes to poke in RAM)
        sta repeat      ; save it 
        ;sta lastnum     ; dubug : to check last pair of bytes
        iny             ; next byte 
        lda (ptr1),y    ; = value to poke
        dey             ; y = 0
poke
        sta RAMWRTON    ; write in aux memory now
        sta (ptr2),y    ; poke byte
        sta RAMWRTOFF   ; write in main memory now
*
incdest
        inc ptr2        ; inc destination address
        bne noincptr
        inc ptr2+1
noincptr 
        dec repeat      ; dec number of bytes to poke
        bne poke        ; loop until repeat  = 0
*
        jsr dec16       ; 16 bit decrement of counter (counter = RLE file size / 2)
        bcc decodexit   ; if carry set : end 
        lda ptr1        ; else read next pair of bytes
        clc             ; ptr1 = ptr1 + 2
        adc #$02
        sta ptr1
        lda ptr1+1
        adc #$00
        sta ptr1+1
        jmp decloop     ; loop with a new pair 
decodexit
        rts
****************** end of decode ****************

dec16
        lda counter
        bne d16
        lda counter+1
        beq zero        ; branch when counter = $0000 (counter is not decremented in that case)
        dec counter+1
d16     dec counter
        lda counter     ; check if the 2 bytes are 0, if so, exit without carry set
        ora counter+1
        beq zero
        sec             ; carry set if counter <> 0
        rts                     
zero    clc             ; carry clear if counter = 0             
        rts
*
*********************** utils ***********************
mygetln                 ; to let user input pattern 
                        ; takes juste upper letters ans ?
                        ; ctrl-c or escape : exit
                        ; return : commit
                        ; delete : delete last char
        lda #$00
        sta pattern     ; pattern length = 0
        sta quitflag

readkeyboard
        lda kbd         ; key keystroke
        bpl readkeyboard
        cmp #$83        ; control-C ?
        bne glnsuite
quif    inc quitflag    ; yes : set quit flag to quit program
        jmp finpat

glnsuite
        cmp #$9b        ; escape ?
        beq quif
        cmp #$8D        ; return ? 
        beq finpat      ; yes : rts
        cmp #$ff        ; delete ? 
        beq delete
        cmp #$88        ; also delete
        beq delete
        and #$7F        ; clear bit 7 for comparisons
        cmp #'?'        ; ? is ok :  represents any char
        beq okchar
        cmp #'A'        ; char between A and  Z are ok
        bcc readkeyboard ; < A : loop
        cmp #'Z'+1
        bcs readkeyboard ; > Z : loop  
okchar  
        ldy pattern     ; pattern must not exceed 15 chars 
        cpy #$0f 
        beq readkeyboard
        pha             ; save char
        ora #$80        ; print it
        jsr cout
        lda ourch       ; get horizontal position
        sta savech      ; save it
        inc pattern     ; pattern length ++
        pla             ; restore char
        ldx pattern     ; poke if in pattern string
        sta pattern,x 
        bit kbdstrb     ; clear kbd
        jmp readkeyboard        ; next char
; delete key
delete  lda pattern     ; get pattern length
        beq readkeyboard        ; if 0 just loop
        dec pattern     ; pattern lenth --
        lda savech      ; savech --
        dec
        sta ourch       ; update h position
        sta savech      ; save it 
        lda #' '        ; print space (to erase previous char)
        ora #$80
        jsr cout
        dec ourch       ; update ourch, so next char will be space was printed
        bit kbdstrb     ; and loop
        jmp readkeyboard

finpat  bit kbdstrb
        rts
*************** end of mygetln ***************


testpat                 ; test if pattern only contains '?'
        ldx pattern
looptp  lda pattern,x ; get a char from pattern
        cmp #'?'
        bne letterfound ; a char is <> from '?'
        dex
        bne looptp
        lda #$01
        sta noletter    ; set flag 
        rts             ; all letters are '?'

letterfound             ; set flag and exit
        lda #$00
        sta noletter
        rts
noletter ds 1



fillmem                 ; fill bitmap1 ($2000 in aux memory) with $ff
                        ; fill bitmap2 ($5000-$8000) with $00
        lda #<bitmap1   ; set bitamp1 address in ptr2 (destination)
        sta ptr2
        lda #>bitmap1
        sta ptr2+1
        jmp fillbmp1
fillbmp1 
        sta RAMWRTON    ; write in AUX memory
        ldy #$00
        lda #$ff        ; fill with $FF, to AND with data to read in index 
fill    sta (ptr2),y
        inc ptr2        ; inc destination address
        bne noincf
        inc ptr2+1 
noincf  
        ldx ptr2+1 
        cpx #$4f      ; $4F28 reached ? (= length of index (12 071) +1)
        bne fill
        ldx ptr2
        cpx #$28
        bne fill
        jsr zerobmap2   ; now empty $5000-$8000 area (bitmap2 area)
        sta RAMWRTOFF
        rts

zerobmap2               ; fill $5000-$8000 with 0
        lda #<bitmap2   ; set bitamp2 address in ptr2 (destination)
        sta ptr2
        lda #>bitmap2
        sta ptr2+1
        ldy #$00
        lda #$00        ; fill with 0
fill0   sta (ptr2),y   
        inc ptr2        ; inc destination address
        bne noincf0
        inc ptr2+1 
noincf0 ldx ptr2+1 
        cpx #$90        ; $9000 reached ?
        bne fill0
        rts   

interpret
* according to a letter, its position in word, and part #,
* set the file name of the corresponding index
* file name format : P<part#>/<letter><pos(in hex)>P<part#>

        lda #'P'
        sta fname+1
        ldx part
        lda tohex,x
        sta fname+2
        lda #'/'
        sta fname+3

        lda #$07
        sta fname
        lda letter
        sta fname+4
        ldx pos
        lda tohex,x
        sta fname+5
        lda #'P'
        sta fname+6
        lda #'0'
        clc
        adc part
        sta fname+7
        rts

multiply
; factors in factor1 and factor2
; result in factor1/factor2 (low/high) 
        lda #0
        ldx #$8
        lsr factor1
multloop
        bcc no_add
        clc
        adc factor2
no_add
        ror
        ror factor1
        dex
        bne multloop
        sta factor2
        rts
factor1 hex 00
factor2 hex 00

print24bits
* prints 3 bytes integer in counter/counter+1/counter+2
* counter+2 must be positive

        lda counter+2        ; init fac with filelength+1/filelength+2
        ldy counter+1        ;
        jsr float               ; convert integer to fac
        jsr mult256             ; * 256
        lda counter          ; add filelength
        jsr dodadd
        jsr PRNTFAC
        rts

mult256
        ldy #>myfac
        ldx #<myfac
        jsr MOVMF       ; fac => memory (packed)
        lda #1
        ldy #0
        jsr float       ; fac = 256
        ldy #>myfac 
        lda #<myfac
        jsr FMULT       ; move number in memory (Y,A) to ARG and mult. result in fac
        rts

dodadd      
        pha 
        ldy #>myfac
        ldx #<myfac
        jsr MOVMF       ; fac => memory (packed)
        ply
        jsr YTOFAC
        ldy #>myfac 
        lda #<myfac
        jsr FADD        ; move number in memory (Y,A) to ARG and add. result in fac
        rts



result  ldx #$00                ; print data read in file (rdbuff = prameter of read mli call)
rslt    lda rdbuff,x
        beq finres              ; exit if char = 0
        ;ora #$80               ; inverse video 
        jsr cout
        inx 
        cpx #reclength          ; no more then record length
        bne rslt
finres  rts


*********** Error processing ***********
ko      pha             ; save error code
        prnstr kolib
        pla
        tax
        jsr xtohex
        cr
        rts

*********** Wait for a key ***********
dowait
        lda kbd
        bpl dowait
        bit kbdstrb
        rts
*
*********** PREFIX *************
doprefix
        jsr MLI           ; getprefix, prefix ==> "path"
        dfb getprefix
        da c7_param
        bcc suitegp
        print kolib
        ;jmp men
        jmp ko 
        ;rts
suitegp
        lda path        ; 1st char = length
        beq noprefix    ; if 0 => no prefix
        jmp good1       ; else prefix already set, exit 

noprefix
        lda devnum      ; last used slot/drive 
        sta unit        ; param du mli online
men     jsr MLI
        dfb online      ; on_line : get prefix in path
        da c5_param
        bcc suite
        print kolib
        ;bra men        ; loop on error  (???)
        jmp ko
suite   lda path
        and #$0f       ; length in low nibble
        sta path
        tax
l1      lda path,x
        sta path+1,x   ; offset 1 byte
        dex
        bne l1
        inc path
        inc path       ;length  +2
        ldx path
        lda #$af       ; = '/'
        sta path,x     ; / after
        sta path+1     ; and / before

        jsr MLI        ; set_prefix
        dfb setprefix
        da c6_param
        bcc good1
        print kolib
good1   
        rts
*
        put bigdisplay.S

********************  disconnect /RAM  **********************
* from : https://prodos8.com/docs/techref/writing-a-prodos-system-program/
* biblio :
* https://www.brutaldeluxe.fr/products/france/psi/psi_systemeprodosdelappleii_ocr.pdf
* or SYSTEME PRODOS DE L'APPLE Il.pdf p.139.

devcnt equ $bf31        ; global page device count
devlst equ $bf32        ; global page device list
machid equ $bf98        ; global page machine id byte
ramslot equ $bf26       ; slot 3, drive 2 is /ram's driver vector in following list :

* ProDOS keeps a table of the addresses of the device drivers assigned to each slot and
* drive between $BF10 and $BF2F. There are two bytes for each slot and drive. $BF10-1F
* is for drive 1, and $BF20-2F is for drive 2. For example, the address of the device
* driver for slot 6 drive 1 is at $BF1C,1D. (Normally this address is $D000.)

*  BF10: Slot zero reserved
*  BF12: Slot 1, drive 1
*  BF14: Slot 2, drive 1
*  BF16: Slot 3, drive 1
*  BF18: Slot 4, drive 1
*  BF1A: Slot 5, drive 1
*  BF1C: Slot 6, drive 1
*  BF1E: Slot 7, drive 1
*  BF20: Slot zero reserved
*  BF22: Slot 1, drive 2
*  BF24: Slot 2, drive 2
*  BF26: Slot 3, drive 2 = I RAM, reserved
*  BF28: Slot 4, drive 2
*  BF2A: Slot 5, drive 2
*  BF2C: Slot 6, drive 2
*  BF2E: Slot 7, drive 2

 * nodev is the global page slot zero, drive 1 disk drive vector.
 * it is reserved for use as the "no device connected" vector.
nodev equ $bf10
 *
ramout
        php             ; save status and
        sei             ; make sure interrupts are off!
 *
 * first thing to do is to see if there is a /ram to disconnect!
 *
        lda machid      ; load the machine id byte
        and #$30        ; to check for a 128k system
        cmp #$30        ; is it 128k?
        bne done        ; if not then branch since no /ram!
 *
        lda ramslot     ; it is 128k; is a device there?
        cmp nodev       ; compare with low byte of nodev
        bne cont        ; branch if not equal, device is connected
        lda ramslot+1   ; check high byte for match
        cmp nodev+1     ; are we connected?
        beq done        ; branch, no work to do; device not there
 *
 * at this point /ram (or some other device) is connected in
 * the slot 3, drive 2 vector.  now we must go thru the device
 * list and find the slot 3, drive 2 unit number of /ram ($bf).
 * the actual unit numbers, (that is to say 'devices') that will
 * be removed will be $bf, $bb, $b7, $b3.  /ram's device number
 * is $bf.  thus this convention will allow other devices that
 * do not necessarily resemble (or in fact, are completely different
 * from) /ram to remain intact in the system.
 *
cont ldy devcnt         ; get the number of devices online
loop lda devlst,y       ; start looking for /ram or facsimile
        and #$f3        ; looking for $bf, $bb, $b7, $b3
        cmp #$b3        ; is device number in {$bf,$bb,$b7,$b3}?
        beq found       ; branch if found..
        dey             ; otherwise check out the next unit #.
        bpl loop        ; branch unless you've run out of units.
        bmi done        ; since you have run out of units to
found lda devlst,y      ; get the original unit number back
        sta ramunitid   ; and save it off for later restoration.
 *
 * now we must remove the unit from the device list by bubbling
 * up the trailing units.
 *
getloop 
        lda devlst+1,y  ; get the next unit number
        sta devlst,y    ; and move it up.
        beq exit        ; branch when done(zeros trail the devlst)
        iny             ; continue to the next unit number...
        bne getloop     ; branch always.
 *
exit    lda ramslot     ; save slot 3, drive 2 device address.
        sta address     ; save off low byte of /ram driver address
        lda ramslot+1   ; save off high byte
        sta address+1   ;
 *
        lda nodev       ; finally copy the 'no device connected'
        sta ramslot     ; into the slot 3, drive 2 vector and
        lda nodev+1     
        sta ramslot+1   
        dec devcnt      ; decrement the device count.
 *
done    plp             ; restore status
 *
        rts             ; and return
 *
address dw $0000      ; store the device driver address here
ramunitid dfb $00     ; store the device's unit number here


*********** DATA ***********

*********** MLI call parameters ***********
quit_parms              ; QUIT call
        hex 04
        hex 0000
        hex 00
        hex 0000
*
c0_parms                ; CREATE file
        hex 07
        da fname        ; path name (same as open)
        hex C3
        hex 00
        hex 0000
        hex 00
        hex 0000
        hex 0000

cb_parms                ; WRITE file
        hex 04
refw    hex 00
datab   hex 0020
lengw   hex 272F
        hex 0000


c1_parms                ; DESTROY file
        hex 01
        da fname        ; path name (same as open)

cc_parms                ; CLOSE file
        hex 01          ; number of params.
        hex 00
*
c8_parms                ; OPEN file for reading             
        hex 03          ; number of params.
        da fname        ; path name
fbuff   hex 0000
ref     hex 00          ; ref ID 
;fname   str "A4P1RL"
fname   ds 16
*
ce_param                ; SET_MARK
        hex 02          ; number of params.
refce   hex 00          ; ref ID
filepos hex 000000      ; new file position
*
ca_parms                ; READ file
        hex 04          ; number of params.
refread hex 00          ; ref #
rdbuffa da rdbuff
rreq    hex 0000        ; bytes requested
readlen hex 0000        ; bytes read
*
rdbuff  ds 256
*
c7_param                ; GET_PREFIX
        hex 01          ; number of params.
        da path
*
c6_param                ; SET_PREFIX
        hex 01          ; number of params.
        da path
*
c5_param                ; ONLINE  
        hex 02          ; number of params.
unit    hex 00
        da path
*
path    ds 256          ; storage for path
*
c4_param                ; GET_FILE_INFO
        hex 0A
        da path
access  hex 00
ftype   hex 00
auxtype hex 0000
stotype hex 00
blocks  hex 0000
date    hex 0000
time    hex 0000
cdate   hex 0000
ctime   hex 0000
*
d1_param                ; GET_EOF
        hex 02
refd1   hex 00
filelength      ds 3

**************************************************
myfac   ds 6            ; to store tempo FAC
counter hex 000000      ; store any counter here
totalcnt hex 000000     ; sum of counters (4 parts) 
recnum  hex 000000
tempo   hex 0000
repeat  hex 00
wordscnt   hex 000000

tohex   asc '0123456789ABCDEF'

letter  ds 1            ; letter 
pos     ds 1            ; position of letter in pattern
part    ds 1            ; part of words list (4 parts)

savech  ds 1
quitflag da 1
savebit ds 1
col     ds 1
pbpos   ds 1

**** strings ****
kolib   str "Error : "
oklib   str "operation ok"
filelib str 'index file : '
totallib str 'Found words : '
patternlib      str 'Enter pattern (A-Z and ?) : '
kopatlib        str 'Error in pattern !'
patlib          str 'Pattern : '
seplib          str ' : '
titlelib        asc ' C R O S S W ? R D   S O L V E R (v. 1.2)'
                hex 00

words           str 'WORDS'

pattern ds 16
refword ds 1
**************************************************
prgend  equ *