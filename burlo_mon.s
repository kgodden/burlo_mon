;
; Kevin Godden 2018
;
; A 6502 monitor program (just for fun!)
;
; To assemble:
;  acme.exe --cpu 6502 --format cbm -o burlo_mon.prg burlo_mon.s
;
;

* = $c000
; Burlo Mon


base    = $02
tmp0    = base
strptr  = base + 1   ; Holds address string buf for printing
addr    = base + 3   ; Holds general purpose memory Address

buf_index = $49
input_buf = $50
input_buf_size = $20

; Some helper macros

; I wish there were PHX, PHY etc!

!macro PHX {
    sta tmp0
    txa
    pha
    lda tmp0
}

!macro PLX {
    sta tmp0
    pla
    tax
    lda tmp0
}
 
!macro PHY {
    sta tmp0
    tya
    pha
    lda tmp0
}

!macro PLY {
    sta tmp0
    pla
    tay
    lda tmp0
}
 
start       
            jsr pwelcome    ; print welcome message
;            jsr pstatus     ; print resister status

            jsr pmemory

            ; Command Loop

command_loop
            ; Loop collecting characters until return/enter is pressed

            jsr gchar           ; Get latest char from input
            beq command_loop    ; 0 indicates no new char
            
            cmp #$13            ; Check for RETURN
            beq RETURN
            ldy buf_index
            iny
            sta input_buf,y
            sty buf_index
            jmp command_loop

RETURN

            ; Zero terminate buffer
            ldy buf_index
            iny
            lda #$00
            sta input_buf,y

            lda #<input_buf
            sta strptr
            lda #>input_buf
            sta strptr + 1

            jsr pstr 
            
            
exit            
            rts
            
            
pmemory     ; Print memory to screen

            lda #$00

            sta addr
            sta addr + 1

            ldy #$08
            
.loop            
            jsr dump8bytes  ; print out 8 bytes worth of memory
            
            lda #$08        ; increase address by 8
            clc
            adc addr        ; add 8 to address  low
            sta addr        ; store it back
            bcc +           ; if carry clear continue
            ldx addr + 1
            inx
            stx addr + 1            
+
            dey
            bne .loop
            
            rts
            
pwelcome
            lda #<welcome
            sta strptr
            lda #>welcome
            sta strptr + 1

            jsr pstr 
            
            rts
            

dump8bytes  ; Dump 8 bytes memory to screen
            ; Start address is stored at addr
            +PHY             ; push Y onto stack so we can restore it later

            ldy #$01
            
.loop0           
            ; First print out start address loop 
            ; the 2 bytes of the address
            lda addr,y
            jsr fhex2
            lda #<strbuf
            sta strptr
            lda #>strbuf
            sta strptr + 1
            jsr pstr 
            dey
            beq .loop0
            
            lda #' '
            jsr pchar
            
            ; Now print out 8 memory values in hex
            ldy #$00

.loop1         
            lda (addr),y
            jsr fhex2

            lda #<strbuf
            sta strptr
            lda #>strbuf
            sta strptr + 1
            jsr pstr 

            lda #' '
            jsr pchar
            
            iny
            cpy #$08
            bne .loop1

            lda #' '        ; space
            jsr pchar
            
            ; Now print out each value as a character or .
  
            ldy #$00
            
.loop2         
            lda (addr),y

            cmp #$20
            bcc .dot
            cmp #$7f
            bcs .dot
            jmp .out
.dot            
            lda #'.'
.out
            jsr pchar

            iny
            cpy #$08
            bne .loop2
            ;;


            lda #$0a        ; linefeed
            jsr pchar
            
            
            +PLY             ; restore y from stack

            rts
            
        

            
pstatus     ; Print register etc. status

            lda #<shead
            sta strptr
            lda #>shead
            sta strptr + 1

            jsr pstr 

            ; PC
            ; ??
            
            ; Stack Register
;            tsx
            txa
            
            
            ; Format as hex string into strbuf
            jsr fhex2
            
            ; Print SR value
            lda #<strbuf
            sta strptr
            lda #>strbuf
            sta strptr + 1

            jsr pstr 
         
            rts
            

;
            
pstr
            +PHY   ; push Y onto stack so we can restore it later
            
            ldy #$00

-           lda (strptr),y
            beq +
            sta $fd
            iny
            jmp -

+ 
            +PLY    ; restore y from stack
            
            rts

pchar

            sta $fd
            rts

gchar
            lda $ff
            rts
            
fhex2
            ; push x onto stack
            +PHX
            
            tax
            lsr
            lsr
            lsr
            lsr
            jsr chex
            sta strbuf
            txa
            and #$0f
            jsr chex
            sta strbuf + 1
            lda #$00
            sta strbuf + 2
            ldx xstore
            
            ; restore x from stack
            +PLX
            
            rts

chex        ; Map number to hex digit
            ; number is in A
            ;sty ystore
            
            ; store y on stack
            +PHY
            
            tay
            lda hexmap,y
            ;ldy ystore
            
            ; restore y from stack
            +PLY
            
            rts            
    
welcome !raw 0x0a, "Welcome to BurloMon v1.0", 0x0a, 0x0a, 0x00
shead !raw "PC   SR   AC   XR    YR   SP", 0x0a, 0x00

strbuf !raw "XXXXXXXXXXXXXXX", 0x00

hexmap !raw "0123456789ABCDEF"

xstore !raw 0x00
ystore !raw 0x00
