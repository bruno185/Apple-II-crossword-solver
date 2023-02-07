* * * * * * * * *
*     MACROS    *
* * * * * * * * *
*
        DO 0
m_inc   MAC         ; inc 16  bits integer
        inc ]1        ; usually a pointer
        bne m_incf
        inc ]1+1
m_incf  EOM
*
*
cr      MAC
        jsr crout
        EOM
*
print   MAC         ; display a 0 terminated string
        ldx #$00      ; in argument
boucle  lda ]1,X
        beq finm
        ora #$80
        jsr cout
        inx
        jmp boucle
finm    EOM 
*
prnstr  MAC             ; display string (length in 1st byte)
        ldy ]1
        beq prnste      ; no char.
        ldx #$00
lpstr   lda ]1+1,X
        ora #$80
        jsr cout
        inx
        dey
        bne lpstr
prnste  EOM
*
getlen  MAC             ; return string length in x
        ldx #$00
loopgetl lda ]1,x
        beq fgetlen
        inx
        jmp loopgetl
fgetlen  EOM
*
printc  MAC             ; dispay a string center
        jmp mainpc
tempo   hex 00
mainpc  getlen ]1       ; of the screen
        txa
        lsr             ; / 2
        sta tempo
        get80
        lda #$14        ; = half line
        bcc pc40
        lda #$28
pc40    sec
        sbc tempo
        tax 
        lda #" "        ; fill with spaces
esp     jsr cout
        dex
        bne esp
        print ]1
        EOM
*
input   MAC             ; input fom user, save str to buffer (arg)  
*                       ; length in first byte, 0 after string
        bit kbdstrb
        jmp insuitem 
savprm  hex 00 
insuitem lda prompt     ; save prompt char.
        sta savprm
        lda #$BE        ; ">"
        sta prompt
        jsr getln 
        txa
        tay             ; save length in Y
        beq l0
inloopm lda $200,X
        sta ]1+1,x
        dex
        cpx #$FF
        bne inloopm
        lda #$00
        sta ]1+1,y     ; zero at the end
l0      sty ]1         ; length in first byte
        lda savprm
        sta prompt
        EOM

closef  MAC             ; close file
        lda ]1
        sta cc_parms+1
        jsr MLI
        dfb close
        da cc_parms
        EOM 

getYN   MAC             ; key = Y or y : Carry = 0
        jsr rdkey       ; otherwise, Carry = 1
        cmp #"Y"
        beq Yes
        cmp #"y"
        beq Yes
        sec
        bcs YNend       ; = jmp
Yes     clc
YNend   nop
        EOM

gotoXY  MAC             ; set cursor position on screen
        lda #]1
        sta ch          ; horizontal
        sta ourch
        lda #]2
        sta cv
        sta ourcv
        jsr vtab        ; vertical
        jsr clreol      ; clear end of line
        EOM
*
get80   MAC             ; 80 col : Carry = 1 
        lda 80col       ; 40 col : Carry = 0
        bmi do80
        clc
        bcc do40        ; = jmp             
do80    sec
do40    EOM    

        FIN