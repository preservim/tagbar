Game Port

;  GAMEPORT.ASM
;

        .MODEL TINY

        .DATA

        yes     DB      13,10,"Game port is installed.",13,10,"$"
        no      DB      13,10,"Game port is not installed.",13,10,"$"

        .CODE
        ORG 100h

start:  mov     al, 1           ;value to write to port
        mov     dx, 201h        ;port number
        out     dx, al          ;write to port
        mov     cx, 0F00h       ;# of loops

port_loop:
        in      al, dx          ;read from port
        and     al, 0Fh         ;if jstick present, then AL should be
        cmp     al, 0Fh         ; 0Fh after ANDing with 0Fh.
        je      jstick_exists
        loop    port_loop
        mov     dx, OFFSET no   ;gameport not installed
        jmp     SHORT done

jstick_exists:
        mov     dx, OFFSET yes  ;gameport installed

done:   mov     ah, 9h
        int     21h

        mov     ax, 4c00h
        int     21h

END     start
