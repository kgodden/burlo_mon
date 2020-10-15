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
strptr  = base + 1      ; Holds address string buf for printing
addr    = base + 3      ; Holds general purpose memory Address

CHAR_IN = $ff

;input_buf_size = $20

; Some helper macros

; I wish there were PHX, PHY etc!

!macro PHX {
    sta tmp_p
    txa
    pha
    lda tmp_p
}

!macro PLX {
    sta tmp_p
    pla
    tax
    lda tmp_p
}
 
!macro PHY {
    sta tmp_p
    tya
    pha
    lda tmp_p
}

!macro PLY {
    sta tmp_p
    pla
    tay
    lda tmp_p
}
 
start       
            jsr pwelcome    ; print welcome message
;            jsr pstatus     ; print resister status

            ;jsr pmemory

            ; Load interrupt vector for monitor
            lda #<break_into
            sta $FFFE
            lda #>break_into
            sta $FFFF
            
            cli     ; Enable interrupts

            brk     ; Jump into the monitor
            rts     ; Should never get here
            
            ; Command Loop

break_into
            ; Dump BRK context so can report it
            stx x_store     ; Store X
            tsx             ; Stack pointer to X
            stx sp_store    ; Store it

            sta a_store     ; Store A
            pla             ; SR
            sta sr_store    ; store it
            pla             ; PCL
            sta pcl_store   ; store it
            pla             ; PCH 
            sta pch_store   ; store it
            sty y_store     ; Don't forget Y!

            cli             ; Re-enable interrupts  (RTI would normally do this for us)

    
            jsr pstatus     ; Print stored status
            
            ; Top of main command loop
            
reset_command_loop
            
            ldx #$00        ; zero terminate buffer
            stx input_buf   
            stx buf_index   ; zero buffer index
            
command_loop
            ; Loop collecting characters until return/enter is pressed

            ;jsr gchar           ; A <-- latest char from input
            lda CHAR_IN
            beq command_loop    ; 0 indicates no new char, loop.
            
            cmp #$0D            ; Check for RETURN
            beq RETURN
            
            jsr pchar           ; echo character
            ldy buf_index       ; Load buffer index
            sta input_buf,y     ; Store char in buffer
            iny                 ; Increase buffer index for next character
            sty buf_index       ; Store buffer index
            
            jmp command_loop    ; Loop for next character

RETURN
            
            ; Zero terminate buffer
            ldy buf_index           ; Current buffer index
            beq reset_command_loop  ; If buffer is empty, loop
            lda #$00
            sta input_buf,y         ; Store 0

            ; output newline
            lda #$0A
            jsr pchar
            
            ;lda #<input_buf
            ;sta strptr
            ;lda #>input_buf
            ;sta strptr + 1
                        
            ;jsr pstr

            ;lda #$0A
            ;jsr pchar
            
            ; Command processing
            
            lda input_buf       ; get first character
            
            cmp #'x'            ; x to exit
            bne +
            rts

+           cmp #'r'
            bne +
            jsr pstatus
            
                

 
+           jmp reset_command_loop
            
            
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
            
pwelcome    ; Print welcome message
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

            ; PC   SR   AC   XR    YR   SP
            ; xxxx xx   xx   xx    xx   xx
            lda #<shead             ; print out header
            sta strptr
            lda #>shead
            sta strptr + 1
            jsr pstr 
            
            ; PC
            
            lda pch_store
            jsr fhex2
            jsr pstrbuf 

            lda pcl_store
            jsr fhex2
            jsr pstrbuf 

            ldy #02
            jsr pspace
            
            ; SR
            
            lda sr_store
            jsr fhex2
            jsr pstrbuf 

            ldy #03
            jsr pspace
            
            ; A

            lda a_store
            jsr fhex2
            jsr pstrbuf 

            ldy #02
            jsr pspace


            ; X

            lda x_store
            jsr fhex2
            jsr pstrbuf 

            lda #' '
            jsr pchar

            ; Y

            lda y_store
            jsr fhex2
            jsr pstrbuf 

            lda #' '
            jsr pchar
            
            ; SP

            lda sp_store
            jsr fhex2
            jsr pstrbuf 
            
            
            lda #$0A            ; Newline
            jsr pchar
            
            rts
            

; Print out string pointed to by (strptr)
            
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

pstrbuf

            lda #<strbuf
            sta strptr
            lda #>strbuf
            sta strptr + 1
            jsr pstr 
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

fillmem     ; Fill memory with Y bytes of A, starting at (strptr)
            cpy #00         ; Check if Y is zero
            beq +           ; Exit if it is
            
            dey             ; Decrease Y so can use it as an index,
                            ; e.g. if write 4 bytes, then start at index 3
            sta (strptr),y  ; Write byte, doesn't affect Z flag
            beq +           ; If Y is 0, exit.
            jmp fillmem     ; Loop
+           rts
            
pspace      ; Print out Y spaces
            lda #' '
            jsr pchar
            dey
            bne pspace
+           rts
            
; Data Section

x_store      !fill 1
y_store      !fill 1
sp_store     !fill 1
a_store      !fill 1
sr_store     !fill 1
pcl_store    !fill 1
pch_store    !fill 1

tmp_p   !fill 1
PCH     !fill 1
PCL     !fill 1
SR      !fill 1
AC      !fill 1
XR      !fill 1
YR      !fill 1
SP      !fill 1

buf_index   !fill 1
input_buf   !fill 40
           
welcome !raw 0x0a, "Welcome to BurloMon v1.0", 0x0a, 0x0a, 0x00
shead !raw " PC   SR   AC   XR    YR   SP", 0x0a, 0x00

strbuf !raw "XXXXXXXXXXXXXXX", 0x00

hexmap !raw "0123456789ABCDEF"

xstore !raw 0x00
ystore !raw 0x00
