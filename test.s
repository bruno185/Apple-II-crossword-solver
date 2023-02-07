
*

*
* prog. equ
pfbuffer    equ $5000   ; storage for PREFIX string (65 bytes max)

fbuff       equ $5100   ; file buffer, 1024 b., for reading
filename    equ $5500   ; storage for filename (65 bytes max)
fbuffW      equ $5600   ; file buffer, 1024 b., for writing

rdbuff      equ $5A00   ; file buffer for reading a file, 512 b.
wrbuff      equ $5C00   ; file buffer for writing a file, 512 b.

ptr1        equ $06
prt2        equ $08
menupos equ $04         ; start display menu on line # 4
menunb  equ $04         ; 4 items in menu
*
*
*

*
* * * * * * * * * *
*    PROGRAMME    *
* * * * * * * * * *
*
        ORG $4000
main    nop
* init
        jsr text
        jsr home
*sta col80off   ; 40 col.
*sta col80on    ; 80 col.
        closef #$00     ; close all files
*
        jsr GetPF
        jsr ShowPF
        jsr ShowMenu
        jsr MenuLoop
        rts

GetPF   nop
        jsr DoPrefix
        rts

ShowPF  nop
        gotoXY 0;0
        prnstr pfbuffer
        ldx pfbuffer    ; length of string
        lda pfbuffer,x  ; last char. 
        cmp #$2F        ; = / ?
        beq spfend
        lda #"/"
        jsr cout
spfend  rts

ShowMenu nop
        jsr title       ; display prog. title
        lda #$01        ; fist item highlighted
        sta curm
        jsr dispmenu
        rts

MenuLoop nop
readkey bit kbdstrb     ; Clear out any data that is already at KBD
keyloop bit kbd
        bpl keyloop
        lda kbd         ; get data (in case)
*
        cmp #$8A        ; down arrow key ?
        beq donext
        bne nextk0
donext  inc curm        ; next nemu item
        lda curm
        cmp #menunb+1   ; last item reached ?
        bne doloop      ; no
        lda #$01        ; yes : reset current highlighted mnu item
        sta curm
        jmp doloop
nextk0  cmp #$95
        bne nextk1
        jmp donext
*
nextk1  nop
        cmp #$8B        ; up arrow key ?
        beq doprev
        bne nextk2
doprev  dec curm
        bne doloop
        lda #menunb
        sta curm
        jmp doloop
nextk2  cmp #$88
        bne nextk3
        jmp doprev

nextk3  nop
        cmp #$8D        ; return key ?
        bne doloop
        jsr docom       ; execute user choice
        lda quit
        beq doloop
        closef #$00     ; close all files
        rts             ; >>> END <<<
*
doloop  jsr dispmenu
        jmp readkey
        rts
*
*  TITLE
*
title   gotoXY 0;1
        ldx #$28
        get80
        bcc dotitle
        ldx #$50        ; 80 char.
dotitle lda #"-"        ; --- line 1
]loop   jsr cout
        dex 
        bne ]loop
        gotoXY 0;2
        printc libtit   ; C r y p t o
        gotoXY 0;3      ; --- line 3       
        ldx #$28
        get80
        bcc line2
        ldx #$50
line2   lda #"-" 
        jsr cout
        dex 
        bne line2 
        rts
*
* PREFIX
*
DoPrefix nop
        jsr  MLI        ; GET PREFIX
        dfb getprefix
        da c7_parms
        bcs dpend       ; error  : exit
        clc             ; because print above may set carry
        ldx pfbuffer    ; get length       
        bne dpend       ; prefix already set : rts
        lda devnum      ; prefix empty : call online
        sta c5_parms+1  ; with last used device 
        jsr MLI         ; ONLINE
        dfb online
        da  c5_parms
        bcs dpend      ; error  : exit
* adjust prefix for set prefix call
        lda pfbuffer    ; get length of prefix
        and #$0F        ; in lower nibble
        tax             ; in x      
        tay             ; saved in y
        inx             ; +1 for /
]loop   lda pfbuffer,y  ; shift string 1 char. right
        sta pfbuffer+1,y
        dey
        bne ]loop
        lda #$2f        ; add  "/" 
        sta pfbuffer+1  ; before prefix 
        stx pfbuffer    ; length in first byte
        jsr MLI         ; SET PREFIX
        dfb setprefix
        da  c6_parms
        bcs dpend      ; error  : exit      
dpend   nop
        rts
*
* * * * * * Display menu routine * * * * * *
*
dispmenu nop
        gotoXY 0;menupos
        ldx #$00
loop    lda mtab,x      ; read menu #
        beq dmend       ; if = 0 then end
        pha             ; save meun #
        inx             ; read string memory location
        lda mtab,x      ; in ptr1
        sta ptr1
        inx
        lda mtab,x
        sta ptr1+1
        pla             ; get menu #
        cmp curm        ; = menu # to hilight ?
        bne prn         ; no : set flag to 0
        lda #$01        ; yes : set flag to 1
        sta invflag
        jmp prn2
prn     lda #$00
        sta invflag
prn2    jsr printm      ; display menu item
        inx
        jmp loop        ; next item
        sta ALTCHARSET0FF
dmend   rts
*
* * * * * * Print menu item * * * * * * 
*
* charsets explained here : 
* https://retrocomputing.stackexchange.com/questions/8652/
* and here :
* http://hackzapple.org/scripts_php/index.php?menu=14&mod=8517283d55e912b0b5ac842147e28904a4a751d3&page=7
*
printm  nop             
        sta ALTCHARSET0N   ; to get lowercase inverse
        lda #$3F
        sta invup+1     ; for 40 col. diplay 
        get80
        bcc prm
        lda #$7F
        sta invup+1     ; for 80 col. diplay         
prm     ldy #$00
lprint  lda (ptr1),y    ; read menu string
        beq prtend    ; ended by 0
        pha             ; save char. to display
        lda invflag     ; normal or inverse ?
        beq normal      ; normal char.
        pla
        cmp #$E0        ; #$E0 = start of lowercase inverse
        bge lower
invup   and #$7F        ; to get inverse uppercase
        jmp out
lower   and #$7F        ; to get inverse lowercase
        jmp out
normal  pla
out     jsr cout
        iny
        jmp lprint
prtend  nop 
        lda #$8D        ; next line
        jsr cout
        rts


* * * * * * * * *
* Do commands   *
* * * * * * * * *
*
docom   nop
        lda curm
        cmp #$01        ; change préfix ?
        bne nextcmd
        jmp ChangePF
nextcmd cmp #$02        ; encrypt file ?
        bne next2
        lda #"K"
        sta letter      ; K to encrypt
        jmp Encrypt
next2   cmp #$03        ; decrypt file ?
        bne next3
        lda #"D"
        sta letter      ; D to decrypt
        jmp Encrypt
next3   lda #$01        ; else : quit
        sta quit
        rts

ChangePF nop
        gotoXY 0;$10
        print newpf     ; invite user
        cr
        input pfbuffer  ; get new prefix from user
        lda pfbuffer    ; get length
        beq okpf
        jsr MLI         ; SET PREFIX
        dfb setprefix
        da  c6_parms
        bcc okpf
        pha
        print err
        pla
        tax 
        jsr xtohex
        jsr rdkey
okpf    jsr DoPrefix
        jsr ShowPF
        gotoXY 0;$10
        jsr clreop
        rts

Encrypt nop             ; Encrypt a file
        lda #$00
        sta secretp     ; init position in secret
        gotoXY 0;$10
        lda letter
        cmp #"K"
        bne decr
        print encryf    ; invite user
        jmp decr2
decr    print decryf
decr2   cr
        input fname
        lda fname       ; get length of filename
        bne :1
        jmp outenc      ; 0 length : exit
:1      jsr MLI         ; OPEN file to encrypt
        dfb open
        da  c8_parms
        bcc okopen      ; open ok ?
doerr   pha             ; no
        print err
        pla 
        tax 
        jsr xtohex
        jsr rdkey
        jmp outenc      ; clear screen and exit
*
okopen  closef #$00     ; close all
        bcc closeok
        jmp doerr
closeok nop             ; output file
        jsr MovefileName  ; copy filename +".K"
        jsr MLI         ; OPEN output file
        dfb open
        da  c8_parmsW
        cmp #$46        ; file not found ?
        beq noexist 
        cmp #$00
        bne doerr       ; use above error management
* file .K exists 
        closef refW     ; close it
        print quest1    ; erase ?
        getYN
        bcc delete
        jmp outenc      ; no : clear screen and exit
delete  jsr MLI
        dfb destroy
        da c1_params
        bcc noexist
        jmp doerr
*
noexist nop
        jsr MLI         ; CREATE output file
        dfb create
        da c0_parms
        bcc closA
        jmp doerr
closA   closef #$00     ; close all
        jsr MLI         ; OPEN output file
        dfb open
        da  c8_parmsW
        lda refW        ; set ref 
        sta refWW       ; for writing
*
        jsr MLI         ; re OPEN file to encrypt
        dfb open
        da  c8_parms
        bcc readf 
        jmp doerr 
readf   jsr RW          ; read, encrypt, write 
        bcc outenc
        cmp #$4C        ; end of file 
        beq outok
        pha 
        gotoXY 0;$10
        print err
        pla 
        tax 
        jsr xtohex
        jsr rdkey
        jmp outenc 
outok   gotoXY 0;$12        ; message file encrypted or decrypted OK
        lda letter
        cmp #"K"
        bne decrok
        print fileok
        jmp decrok2
decrok  print fileokd
decrok2 jsr clreol
        jsr rdkey
outenc  gotoXY 0;$10        ; claer screen and return
        jsr clreop
        closef #$00
        rts

*
MovefileName ldy fname  ; filename ==> filename.K 
        ldx #$00
movefn  lda fname+1,x 
        sta fnameW+1,x
        inx
        dey
        bne movefn
        lda #"."
        sta fnameW+1,x
        inx 
        lda letter
        sta fnameW+1,x
        lda fname
        clc
        adc #$02
        sta fnameW
        rts
*
RW      nop             ; read, encrypt, write
        lda ref
        sta ca_parms+1  ; ref. of file to read
        jsr MLI         ; READ 256 bytes
        dfb read
        da  ca_parms 
        cmp #$4C        ; end of file ?
        beq RWex        ; yes : return
        bcc rw2         ; no (and no error)
RWex    rts             ; exit if end of file or error

rw2     nop
        jsr docode
        lda readlen     ; set bytes to write = bytes read
        sta reql
        lda readlen+1
        sta reql+1        
        jsr MLI         ; WRITE 
        dfb write
        da  cb_parms
        bcs RWex        ; end of file if Carry set (or error)
        jmp RW
*
docode  nop             ; encrypt file buffer 
        lda readlen     ; check > 0 bytes read
        ora readlen+1
        beq outdocode   ; if 0 : exit
        lda #$00        ; init. 16 bits counter
        sta cpt16
        sta cpt16+1
*
charge  lda rdbuff      ; read byte in buffer
        pha             ; save it
code    ldx secretp     ; get position in secret
        lda secret,x    ; get secret byte
        sta eorval      ; store it for later eor
        bne cod2        ; end of secret not reached
        sta secretp     ; end of secret reached : pos = 0
        jmp code        ; reload secret byte
cod2    pla             ; pop byte read  
        inc secretp     ; next byte in secret
        eor eorval      ; encrypt !!
doout   sta wrbuff      ; store in buffer to write   
        m_inc cpt16     ; counter +=2
selfmod m_inc charge+1  ; self modify load address
self2   m_inc doout+1   ; self modify store address
compar  lda cpt16+1     ; compare counter
        cmp readlen+1   ; with bytes read
        bne charge
        lda cpt16
        cmp readlen
        bne charge      ; loop if counter <> bytes read
*
        lda #<rdbuff    ; restore buffers addresses
        sta charge+1    ; for further calls
        lda #>rdbuff
        sta charge+2
        lda #<wrbuff
        sta doout+1
        lda #>wrbuff
        sta doout+2
outdocode rts

*
**********************************       
*              DATA              *
********************************** 
c0_parms                ; create file
        hex 07
        da fnameW       ; file name
        hex C3          ; access bits
        hex 06          ; file type = BIN
        hex 0000        ; aux file type
        hex 01          ; storage type
        hex 0000        ; date
        hex 0000        ; time

c1_params               ; destroy file
        hex 01
        da fnameW       ; file name

c5_parms                ; online
        hex 02
        hex 00
        da  pfbuffer

c6_parms                ; set_prefix
        hex 01
        da pfbuffer


c7_parms                ; get_prefix
        hex 01
        da pfbuffer

c8_parms                ; open file for reading             
        hex 03
        da fname
        da fbuff
ref     hex 00

c8_parmsW                ; open file for writing             
        hex 03
        da fnameW
        da fbuffW
refW    hex 00

ca_parms                ; read file
        hex 04          ; number of params.
        hex 00          ; ref #
        da rdbuff
        hex 0001        ; 256 b. requested
readlen hex 0000        ; b. read

cb_parms                ; write file
        hex 04
refWW   hex 00          ; file reference
        da wrbuff       ; buffer for data to be written
reql    hex 0001        ; 256 b. requested
actl    hex 0000        ; number of byte written


cc_parms                ; close file
        hex 01
        hex 00

fname ds 65,0
fnameW ds 65,0

libtit  asc "C r y p t o"
        hex 00
*
secret  asc "when I'm 64"
        hex 00
secretp hex 00          ; position in secret (offset)
cpt16   hex 0000
eorval  hex 00
letter  hex 00          ; "K" to encrypt or "D" to decrypt
*
err     asc "I/O Error : "
        hex 00

quit    hex 00          ; quit flag  

newpf   asc "Enter new prefix :"
        hex 00
encryf  asc "Type the name of the file to encrypt :"
        hex 00
quest1  asc "File exists. Overwrite (Y/N) ?"
        hex 00
fileok  asc "File encrypted !"
        hex 00
decryf  asc "Type the name of the file to decrypt :"
        hex 00   
fileokd asc "File decrypted !"
        hex 00   
*
* * * * * * * * DATA  * * * * * * * * 
*  needed to dispalay menu routine  *
* * * * * * * * * * * * * * * * * * * 
mtab    hex 01
        da m1
        hex 02
        da m2
        hex 03
        da m3
        hex 04
        da m4
        hex 00          ; flog for end of array

curm    hex 00
invflag hex 00


m1      asc "Change PREFIX"
        hex 00
m2      asc "Encrypt a file"
        hex 00
m3      asc "Decrypt a file"
        hex 00
m4      asc "Exit"
        hex 00 


