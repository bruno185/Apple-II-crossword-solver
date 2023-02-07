****************************************************
dec24
        lda counter     ; do 24-bit decrement
        bne d6
        lda counter+1
        bne d5
        lda counter+2
        beq dquitf      ; if all bytes are 0 : exit
        dec counter+2
d5      dec counter+1
d6      dec counter
        lda counter             
        ora counter+1   ; check if the 3 bytes are 0, if so, exit without carry set
        ora counter+2
        beq dquitf 
        sec
        rts
dquitf  clc             ; carry = 0 ==> filelength = 0
        rts

****************************************************
* get RAM from ProDOS

        lda #4          ; ask 4 pages of ram
        jsr GETBUFR     ; to prodos
        bcc okbuffer    ; error ?
        jmp ko
okbuffer

        lda #48          ; ask 4 pages of ram
        jsr GETBUFR      ; to prodos 
        bcc ok481
        jmp ko
ok481
        lda #48          ; ask 4 pages of ram
        jsr GETBUFR      ; to prodos 
        bcc ok482
        jmp ko
ok482
        lda #28          ; ask 4 pages of ram
        jsr GETBUFR      ; to prodos 
        bcc ok483
        jmp ko

****************************************************
concat                  
* concat 2 strings (with 1st byte = length)
        ldx #$00
        lda str1        ; get string1 length
        pha             ; save it
        clc             ; add string2 length
        adc str2
        sta str3
        beq concatend   ; both string empty : exit
        pla             ; restore string1 length
        beq concat2     ; = 0 : just copy string2 to result
        ldx #$00
cloop1
        lda str1+1,x    ; get a char from string1 
        sta str3+1,x    ; put it in result
        inx 
        cpx str1        ; end ?
        bne cloop1      ; no : loop
concat2
        ldy #$00
cloop2
        lda str2+1,y
        sta str3+1,x
        inx
        iny
        cpy str2 
        bne cloop2
concatend
        rts
str1    str 'abc'
str2    str 'def'
str3    ds 16


****************************************************
* disconnect /RAM disk
* prodos_technical_notes.pdf
* About $BF10

devcnt equ $BF31
devlst equ $BF32
devadr equ $BF10
ramout
        lda #$B0        ; slot3,drive2
devget  sta unitno ; store for later compare instruction
        ldx devcnt ; get count-1 from $BF31
devloop lda devlst,x ; get entry in list
        and #$F0 ; mask off low byte
devcomp cmp unitno ; compare to the unit_number we filled
in      beq goodn ;
        dex
        bpl devloop ; loop again if still less than $80
        bmi outtt ; error: bad unit number
goodn   lda unitno ; get good copy of unit_number
        lsr a ; divide it by 8
        lsr a ; (not sixteen because devadr entries are
        lsr a ; two bytes wide)
        tax
        lda devadr,x ; low byte of device driver address
        sta addr
        lda devadr+1,x ; high byte of device driver address
        sta addr+1
        tax 
        jsr xtohex
        ldx addr
        jsr xtohex
outtt   rts
addr dw 0 ; address will be filled in here by
goodnum
unitno dfb 0 ; unit number storage


****************************************************
; quit call
        jsr MLI         ; quit mli call
        dfb quit
        da quit_parms

quit_parms
        hex 04
        hex 0000
        hex 00
        hex 0000
****************************************************

        lda prompt
        pha
        lda #' '       ; 
        lda prompt
        jsr getln1
        pla
        sta prompt

****************************************************
displayw
        
        beq zerobyte
        ldy #$08
dolsr   
        inc wordscnt    ; inc word counter
        bne dolsr1
        inc wordscnt+1
        bne dolsr1
        inc wordscnt+2
dolsr1  lsr             ; get bit in carry
        bcs bitfound
        dey
        bne dolsr
        rts
bitfound
        ldx #$02
copywc  lda wordscnt,x 
        sta wordscnt2,x 
        dex
        bpl copywc
        ldx #$04
mul16wc asl wordscnt2
        rol wordscnt2+1
        rol wordscnt2+2
        dex
        bne mul16wc 
        lda wordscnt2
        sta filepos
        lda wordscnt2+1
        sta filepos+1
        lda wordscnt2+2
        sta filepos+2
**** mli set_mark
**** read
**** rint
        rts

zerobyte
        lda wordscnt    ; word counter : +8
        clc
        adc #$08
        sta wordscnt
        lda #$00
        adc wordscnt+1
        sta wordscnt+1
        lda #$00
        adc wordscnt+2
        sta wordscnt+2
        rts


        ldx #$00        ; print found word
prnword lda rdbuff,x 
        beq eoword
        ora #$80
        jsr cout
        inx 
        jmp prnword


***************************************************************************
***************************************************************************

* set mark              ; set position in file for a specific record
        lda ref         ; get file ref id
        sta refce       ; set ref id for set_mark mli call
        sta refread     ; set ref id for read mli call
        jsr setpos
        jsr MLI
        dfb setmark
        da ce_param     ; set_mark
        bcs ko
        jsr dook

*  read   
        lda #<reclength
        sta rreq        ; set requested bytes
        lda #>reclength
        sta rreq+1
        jsr MLI         ; read  bytes
        dfb read
        da  ca_parms
        bcs ko
        jsr dook

        jsr result      ; print read bytes
        rts

        lda #$00
        sta $BF94
        closef #$00     ; close all files
        rts

ko      pha             ; save error code
        prnstr kolib
        pla
        tax
        jsr xtohex
        cr
dowait
        lda kbd
        bpl dowait
        bit kbdstrb
        rts
*
dook    prnstr oklib
        cr
        rts
        
****************

setpos  lda recnum              ; set_mark=recnum * 8  
        sta filepos             ; init filepos with recnum
        lda recnum+1            
        sta filepos+1
        lda recnum+2
        sta filepos+2
        ldy #$04                ; multiply by 16
mult    asl filepos             ; 16 = 2^4 
        rol filepos+1
        rol filepos+2
        dey
        bne mult
        rts

        sta filepos
        lda #$00 
        sta filepos+1