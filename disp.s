**********************
* DISPAY found words *
**********************
*
displayw
        lda counter    ; words found for this part total = 0 ?
        ora counter+1
        ora counter+2
        bne adjustcnt   ; no : go on
        rts
        ;jmp showr       ; yes show result

adjustcnt
* adjust wordscnt because last byte of a part is not significant partly
        ldy part
        dey
        beq go  ; part 1 : let wordscnt be 0

        lda #$01        ; add $17936 (=96566) to counter
        sta wordscnt+2
        lda #$79
        sta wordscnt+1
        lda #$36
        sta wordscnt

dodey   dey
        beq go 
        lda wordscnt    ; add again for each part 
        clc
        adc #$36
        sta wordscnt
        lda wordscnt+1
        adc #$79
        sta wordscnt+1
        lda wordscnt+2
        adc #$01
        sta wordscnt+2  
        jmp dodey

go
        closef #$00     ; close all files
        jsr FREEBUFR    ; free all buffers

*** WORDS file
        ldx words       ; open WORDS file
        stx fname       ; copy file name from to fname mli parameter 
copyfn  lda words,x 
        sta fname,x 
        dex
        bne copyfn

        lda #$00        ; set buffer for WORDS files : $8800
        sta fbuff
        lda #$88
        sta fbuff+1
        jsr MLI         ; open WORDS file
        dfb open
        da  c8_parms

        lda ref
        sta refword     ; save ref ID of WORDS file.

        jsr settempofn  ; set TEMPOx file name in fname (param of open mli call)
        jsr setopenbuffer ; get free mem from ProDOS for OPEN call (mli)  
        jsr MLI         ; open tempo index file
        dfb open
        da  c8_parms

*** TEMPO index file 
        lda ref         ; get ref from previous OPEN call
        sta refd1
        jsr MLI         ; get file length 
        dfb geteof
        da d1_param

        jsr readindex   ; prepare loading of index file
        jsr MLI         ; load file in main memory ($2000)
        dfb read
        da  ca_parms

        lda refd1       ; close TEMPO file
        sta cc_parms+1
        jsr MLI
        dfb close
        da cc_parms


*** process index 
        lda #>bitmap1   ; set pointer to $2000 area
        sta ptr1+1
        lda #<bitmap1
        sta ptr1

loopreadbyte
        ldy #$00
        lda (ptr1),y    ; get byte to read
        sta tempo
        bne nonzero
        jmp zerobyte

nonzero ldy #$08
        sty savebit 
dolsr   lsr tempo
        bcs bitfound

nextbit 
        jsr incwrdcnt   ; word counter++
        dec savebit     ; dec number of bits to scan
        bne dolsr       ; not 8 bits yet : loop
* inc ptr
        jmp eoword3     ; update pointers for next byte 

bitfound
*** set_mark call
        ldx #$02
copywc  lda wordscnt,x  ; copy word counter to filepos param for set-mark call param
        sta filepos,x 
        dex
        bpl copywc
        ldx #$04
mul16wc asl filepos     ; filepos = filepos * 16 (16 char per word in words file)
        rol filepos+1
        rol filepos+2
        dex
        bne mul16wc 

        lda refword     ; copy file ID to set-mark call param
        sta refce       ; to set-mark call param
        sta refread     ; and to read call param
        jsr MLI         ; set_mark call
        dfb setmark
        da ce_param
*** read a word
        lda #16
        sta rreq        ; 16 bytes to read
        lda #$00
        sta rreq+1    
        lda #<rdbuff    ; set data buffer for reading file
        sta rdbuffa
        lda #>rdbuff+1
        sta rdbuffa+1       
        jsr MLI         ; load word
        dfb read
        da  ca_parms

*** print word
        jsr result      ; print data read in word file   
        lda col         ; adjust position on screen for next word
        cmp #64         ; 64 horizontal = last posiotn on line           
        beq lastcol     
        clc             ; enough room opu next word 
        adc #16         ; move horizontal posiiton 16 rows to the right 
        jmp outscr
lastcol                 
        cr              ; last horizontal posiiton on screen
        lda #$00        ; reset horizontal posiiton    
outscr  sta col         ; store in col var
        sta ourch       ; set value for rom/prodos routine

eoword  
        jsr incwrdcnt
        dec savebit 
        bne dolsr
        jmp eoword3
*** end of LSR loop 

eoword3       
        inc ptr1
        bne noinc2
        inc ptr1+1
noinc2  
        lda ptr1        ; 12071 byte to scan  + 1 (because of inc) = $2F28
        cmp #$28        ; from $2000 to $2000+$2F28 = $4F28
        beq chechhi
        jmp loopreadbyte
chechhi lda ptr1+1
        cmp #$2F + #$20
        beq dispexit
        jmp loopreadbyte
dispexit
        lda #$00
        sta $BF94
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
        jmp eoword3     ; next byte

************** end of displayw **************


***
incwrdcnt 
        inc wordscnt    ; inc word counter
        bne nowinc1
        inc wordscnt+1
nowinc1 bne incfin
        inc wordscnt+2
incfin  rts