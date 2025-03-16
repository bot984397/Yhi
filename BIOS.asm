   ;-----------------------------------------------------------------------;    
   ;          EXTENDED IBM 5150 BIOS IMPLEMENTATION - EXPERIMENTAL         ;             
   ;      CHANGE YHI_BOOT_SECURE IN DATA SECTION TO ENABLE SECURE BOOT     ;
   ;                                                                       ;
   ; AUTHOR:   VMX                                                         ;
   ; DATE:     10/03/2025                                                  ;
   ;                                                                       ;
   ; "Every byte is sacred, every byte is great.                           ;
   ;  If a byte is wasted, God gets quite irate."                          ;
   ;-----------------------------------------------------------------------;

[BITS 16]
[ORG 0E000H]

%define BIOS_SEG  0F000H
%define STACK_SEG 00030H
%define BDA_SEG   00040H

%MACRO PROC 1
   %1:
%ENDMACRO

%MACRO ENDP 1
%ENDMACRO

   ;-----------------------------------------------------------------------;    
   ;                         CONSTANTS AND EQUATES                         ;                            
   ;-----------------------------------------------------------------------;

%define ROM_SCAN_START  0C000H
%define ROM_SCAN_END    0F000H
%define ROM_SIGNATURE1  055H
%define ROM_SIGNATURE2  0AAH
%define ROM_SEGMENT_INC 080H

;BDA_SEG           EQU 0x0040
struc BDA
   .NETBOOT_PRESENT  RESB 1
   .RESET_FLAG       RESW 1
   .IO_RAM_SIZE      RESW 1
endstruc

PPI_PORT_A        EQU 60H
PPI_PORT_B        EQU 61H
PPI_PORT_C        EQU 62H
PPI_CTRL          EQU 63H
PIC_CMD           EQU 20H
PIC_DATA          EQU 21H
PIT_COUNTER_0     EQU 40H
PIT_COUNTER_1     EQU 41H
PIT_COUNTER_2     EQU 42H
PI_CTRL           EQU 43H

TIMER             EQU 40H
TIMER_CTRL        EQU 43H
TIMER_0           EQU 40H
DMA_PORT_08       EQU 08H
DMA               EQU 00H
   ;-----------------------------------------------------------------------;    
   ;                           KEYBOARD DATA AREA                          ;                             
   ;-----------------------------------------------------------------------;
KBD_FLAG_0        DB ?                    ; FIRST KEYBOARD STATUS BYTE

INSERT_STATE      EQU 80H                 ; 1000_0000
CAPS_STATE        EQU 40H                 ; 0100_0000
NUM_STATE         EQU 20H                 ; 0010_0000
SCROLL_STATE      EQU 10H                 ; 0001_0000
ALT_SHIFT_STATE   EQU 08H                 ; 0000_1000
ALT_CTL_STATE     EQU 04H                 ; 0000_0100
LEFT_SHIFT_STATE  EQU 02H                 ; 0000_0010
LEFT_CTL_STATE    EQU 01H                 ; 0000_0001

KBD_FLAG_1        DB ?                    ; SECOND KEYBOARD STATUS BYTE

INSERT_SHIFT      EQU 80H                 ; 1000_0000
CAPS_SHIFT        EQU 40H                 ; 0100_0000
NUM_SHIFT         EQU 20H                 ; 0010_0000
SCROLL_SHIFT      EQU 10H                 ; 0000_1000
HOLD_STATE        EQU 08H                 ; 0000_0100

KBD_BUF_HEAD      DW ?                    ; POINTER TO HEAD OF BUFFER
KBD_BUF_TAIL      DW ?                    ; POINTER TO TAIL OF BUFFER
KBD_BUF           DW 16 DUP(?)            ; ROOM FOR 16 ENTRIES
KBD_BUF_END       EQU KBD_BUF + 32        ; END OF KEYBOARD BUFFER

YHI_BOOT_SECURE   EQU 01H                 ; ENABLE/DISABLE SECUREBOOT
BIOS_SIGNATURE    EQU 0AAH                ; BIOS SIGNATURE

   ;-----------------------------------------------------------------------;    
   ;                         SUBROUTINES - UTILITY                         ;                            
   ;-----------------------------------------------------------------------;

   ;----- LOAD BIOS DATA AREA ---------------------------------------------;
   ; THIS ROUTINE SETS UP (ES) to point to the BDA                         ;
   ; INPUT                                                                 ;
   ;     NO REGISTERS                                                      ;
   ; OUTPUT                                                                ;
   ;     (ES) SET TO BDA (BIOS DATA AREA)                                  ;
   ;-----------------------------------------------------------------------;
PROC LDBDA
   PUSH     AX                            ; SAVE AX
   MOV      AX,BDA_SEG                    ; LOAD BDA SEGMENT INTO AX
   MOV      ES,AX                         ; LOAD BDA SEGMENT INTO ES
   POP      AX                            ; RESTORE AX
   RET                                    ; RETURN TO CALLER
ENDP LDBDA

PROC STGTST
   RET
ENDP STGTST

   ;----- CHECK BOOT KEYS -------------------------------------------------;
   ; THIS ROUTINE CHECKS IF KEY COMBINATIONS ARE PRESSED                   ;
   ; INPUT                                                                 ;
   ;     NO REGISTERS                                                      ;
   ; OUTPUT                                                                ;
   ;     (AL)=0   NO KEYS FOUND                                            ;
   ;     (AL)=1   SECUREBOOT MANAGER [CTRL+ALT+S]                          ;
   ;     (AL)=2   NETBOOT MANAGER [CTRL+ALT+N]                             ;
   ; NOTE                                                                  ;
   ;     IF BOTH [S] and [N] are pressed with [CTRL+ALT], THE SECUREBOOT   ;
   ;     INTERFACE TAKES PRIORITY.                                         ;
   ;-----------------------------------------------------------------------;
PROC YHI_CHECK_BOOT_KEYS
   MOV      AH,01H                        ; KEYBOARD STATUS
   INT      16H
   JZ       NO_KEYS
   MOV      AH,02H                        ; READ SHIFT STATUS
   INT      16H
   TEST     AL,0CH                        ; ARE CTRL + ALT PRESSED?
   JZ       NO_KEYS                       ; NO? EXIT
   MOV      AH,00H                        ; READ KEY FROM BUFFER
   INT      16H
   AND      AL,0DFH                       ; CONVERT TO UPPERCASE
   CMP      AL,'S'                        ; SECUREBOOT SEQUENCE
   MOV      AL,1H
   JE       CHECK_END
   CMP      AL,'N'                        ; NETBOOT SEQUENCE
   INC      AL
   JE       CHECK_END
NO_KEYS:
   XOR      AL,AL
CHECK_END:
   RET                                    ; RETURN TO CALLER
ENDP YHI_CHECK_BOOT_KEYS

PROC YHI_CHECK_FIRST_BOOT
   ; CMP DWORD[ADMIN_PASSWORD_HASH], 0FFFFFFFFh
   JE       FIRST_TIME_SETUP
   RET
FIRST_TIME_SETUP:
   ;MOV      SI,FIRST_BOOT_MSG
   ;CALL     PRINT_STRING                  ; DISPLAY FIRST BOOT MESSAGE
   ;MOV      SI,PASSWORD_PROMPT
   ;CALL     PRINT_STRING                  ; DISPLAY PASSWORD PROMPT
   ;MOV      DI,PASSWORD_BUFFER
   ;CALL     GET_PASSWORD                  ; READ PASSWORD FROM USER
   ;MOV      SI,PASSWORD_BUFFER
   ;CALL     HASH_PASSWORD                 ; HASH NEW PASSWORD (MD4)
   ; MOV [ADMIN_PASSWORD_HASH],AX
   ; MOV [ADMIN_PASSWORD_HASH+2],DX
   ; MOV BYTE[SECUREBOOT_ENABLED],1
   ; MOV BYTE[HASH_COUNT],0
   ;MOV      SI,SETUP_COMPLETE_MSG
   ;CALL     PRINT_STRING                  ; PRINT COMPLETION MESSAGE
   MOV      AH,00H                        ; WAIT FOR KEYPRESS
   INT      16H
   RET                                    ; RETURN TO CALLER
ENDP YHI_CHECK_FIRST_BOOT

   ;----- ROM SCAN --------------------------------------------------------;
   ; THIS ROUTINE SCANS THE C000:0000-F000:0000 SPACE FOR EXPANSION ROMS   ;
   ; INPUT                                                                 ;
   ;     NO REGISTERS                                                      ;
   ; OUTPUT                                                                ;
   ;     NO REGISTERS                                                      ;
   ;     VARIABLES IN THE DATA SECTION ARE SET ACCORDINGLY                 ;
   ;-----------------------------------------------------------------------;
PROC ROM_SCAN
   MOV      AX,ROM_SCAN_START             ; START AT C000:0000
   MOV      ES,AX                         ; SEG: C000H
   XOR      DI,DI                         ; OFF: 0000H
ROM_SCAN_LOOP:
   CMP      BYTE [ES:00H],ROM_SIGNATURE1  ; BYTE 1: 055H
   JNE      ROM_SCAN_NEXT
   CMP      BYTE [ES:01H],ROM_SIGNATURE2  ; BYTE 2: 0AAH
   JNE      ROM_SCAN_NEXT
   MOVZX    CX,BYTE[ES:02H]               ; GET ROM SIZE IN 512-BYTE BLOCKS
   SHL      CX,01H                        ; CONVERT TO 256-BYTE BLOCKS
   XOR      AL,AL
   XOR      BX,BX
ROM_SCAN_CHECKSUM_LOOP:
   ADD      AL,BYTE[ES:BX]                ; COMPUTE CHECKSUM
   INC      BX
   LOOP     ROM_SCAN_CHECKSUM_LOOP
   TEST     AL,AL                         ; ALL BYTES SHOULD SUM UP TO 0
   JNZ      ROM_SCAN_NEXT                 ; SKIP ROM - INVALID CHECKSUM
   PUSH     AX                            ; SAVE REGISTERS BEFORE FAR CALL
   PUSH     BX
   PUSH     CX
   PUSH     DX
   PUSH     SI
   PUSH     DI
   PUSH     BP
   PUSH     DS
   PUSHF
   CALL     FAR[ES:03H]                   ; FAR CALL TO ROM INIT FUNCTION
   CMP      AX,'BN'                       ; NETBOOT EXPANSION ROM
   JNE      ROM_SCAN_CONT                 ; NO? CONTINUE
   PUSH     ES
   CALL     LDBDA                         ; LOAD BDA SEGMENT
   MOV      BYTE[ES:BDA.NETBOOT_PRESENT],01H
   POP      ES
ROM_SCAN_CONT:
   POP      DS                            ; RESTORE REGISTERS AFTER FAR CALL
   POP      BP
   POP      DI
   POP      SI
   POP      DX
   POP      CX
   POP      BX
   POP      AX
ROM_SCAN_NEXT:
   MOV      AX,ES                         ; MOVE TO NEXT 2KB BOUNDARY
   ADD      AX,ROM_SEGMENT_INC            ; ADD 2KB (80H PARAGRAPHS)
   MOV      ES,AX
   CMP      AX,ROM_SCAN_END               ; CHECK IF WE'VE REACHED THE END
   JB       ROM_SCAN_LOOP
   RET                                    ; RETURN TO CALLER
ENDP ROM_SCAN

   ;-----------------------------------------------------------------------;    
   ;                      INTERRUPT VECTOR TABLE SETUP                     ;                        
   ;-----------------------------------------------------------------------;
PROC SETUP_IVT
   XOR      AX,AX
   MOV      ES,AX

   ;MOV      WORD[ES:10H*4], INT_10        ; INT 10H - VIDEO SERVICES
   MOV      WORD[ES:10H*4+2], 0F000H
   ;MOV      WORD[ES:13H*4], INT_13        ; INT 13H - DISK SERVICES
   MOV      WORD[ES:13H*4+2], 0F000H
   MOV      WORD[ES:16H*4], INT_16        ; INT 16H - KEYBOARD SERVICES
   MOV      WORD[ES:16H*4+2], 0F000H
   ;MOV      WORD[ES:18H*4], INT_18        ; INT 18H - ROM BASIC
   MOV      WORD[ES:18H*4+2], 0F000H
   MOV      WORD[ES:19H*4], INT_19        ; INT 19H - BOOTSTRAP LOADER
   MOV      WORD[ES:19H*4+2], 0F000H
ENDP SETUP_IVT

   ;-----------------------------------------------------------------------;    
   ;                       SUBROUTINES - CRYPTOGRAPHY                      ;                         
   ;-----------------------------------------------------------------------;

   ;-----------------------------------------------------------------------;    
   ;                    RESET VECTOR - BIOS ENTRY POINT                    ;                       
   ;-----------------------------------------------------------------------;
PROC START
   ;----- INTEL 8088 PROCESSOR TESTS --------------------------------------;
   ; VALIDATE 8088 FLAGS, REGISTERS AND CONDITIONAL BRANCHING              ;
   ;-----------------------------------------------------------------------;
   CLI                                    ; DISABLE INTERRUPTS
   MOV      AH,0D5H                       ; SET SF, CF, ZF, AF
   SAHF                                   ; STORE AH INTO FLAGS
   JNC      ERR01                         ; ERR IF CF CLEAR
   JNZ      ERR01                         ; ERR IF ZF CLEAR
   JNP      ERR01                         ; ERR IF PF CLEAR
   JNS      ERR01                         ; ERR IF SF CLEAR
   LAHF                                   ; LOAD FLAGS INTO AH
   MOV      CL,5                          ; LOAD COUNTER WITH SHIFT COUNT
   SHR      AH,CL                         ; SHIFT AH LEFT (CL) TIMES
   JNC      ERR01                         ; ERR IF AF CLEAR
   MOV      AL,40H                        ; SET OF FLAG
   SHL      AL,1                          ; SHIFT AH LEFT BY 1
   JNO      ERR01                         ; ERR IF OF CLEAR
   XOR      AH,AH                         ; CLEAR AH
   SAHF                                   ; STORE AH INTO FLAGS (CLEAR FLAGS)
   JBE      ERR01                         ; ERR IF CF SET
   JS       ERR01                         ; ERR IF SF SET
   JP       ERR01                         ; ERR IF PF SET
   LAHF                                   ; LOAD FLAGS INTO AH
   MOV      CL,5                          ; LOAD COUNTER WITH SHIFT COUNT
   SHR      AH,CL                         ; SHIFT AH RIGHT (CL) TIMES
   JC       ERR01                         ; ERR IF CF SET
   SHL      AH,1                          ; SHIFT AH LEFT BY 1
   JO       ERR01                         ; ERR IF OF SET

   MOV      AX,0FFFFH                     ; SET ALL AX BITS TO 1
   STC                                    ; SET CARRY FLAG
CPUTEST_1:
   MOV      DS,AX                         ; LOAD AX INTO DS
   MOV      BX,DS                         ; LOAD DS INTO BX
   MOV      ES,BX                         ; LOAD BX INTO ES
   MOV      CX,ES                         ; LOAD ES INTO CX
   MOV      SS,CX                         ; LOAD CX INTO SS
   MOV      DX,SS                         ; LOAD SS INTO DX
   MOV      SP,DX                         ; LOAD DX INTO SP
   MOV      BP,SP                         ; LOAD SP INTO BP
   MOV      SI,BP                         ; LOAD BP INTO SI
   MOV      DI,SI                         ; LOAD SI INTO DI
   JNC      CPUTEST_2                     ; FINISH IF CARRY FLAG CLER
   XOR      AX,DI                         ; DID 1 PATTERN MAKE IT THROUGH?
   JNZ      ERR01                         ; NO? ERR
   CLC                                    ; CLEAR CARRY FLAG
   JMP      CPUTEST_1
CPUTEST_2:
   OR       AX,DI                         ; DID 0 PATTERN MAKE IT THROUGH?
   JZ       DMATEST_1                     ; YES? CONTINUE
ERR01:
   HLT

   ;----- INTEL 8237 DMA TESTS --------------------------------------------;
   ; DISABLE THE 8237 DMA CONTROLLER.                                      ;
   ; VALIDATE TIMER 1.                                                     ;
   ; READ/WRITE CURRENT ADDRESS AND WORD COUNT FOR ALL CHANNELS.           ;
   ; INITIALIZE AND START DMA CONTROLLER.                                  ;
   ;-----------------------------------------------------------------------;
DMATEST_1:
   MOV      AL,04H                        ; DISABLE DMA CONTROLLER
   OUT      DMA_PORT_08,AL
   ;-----------------------------------------------------------------------;
   ; VALIDATE TIMER 1                                                      ;
   ;-----------------------------------------------------------------------;
   MOV      AL,54H                        ; TIMER 1, LSB, MODE 2
   OUT      TIMER+3,AL
   MOV      AL,CL                         ; INITIAL TIMER COUNT TO 0
   OUT      TIMER+1,AL
TIMERTEST_1:
   MOV      AL,40H                        ; LATCH TIMER 1 COUNT
   OUT      TIMER+3,AL
   CMP      BL,0FFH                       ; CHECK IF ALL BITS CLEAR
   JE       TIMERTEST_2
   IN       AL,TIMER+1                    ; READ TIMER 1 COUNT
   OR       BL,AL                         ; SET ALL BITS IN TIMER
   LOOP     TIMERTEST_1
   HLT                                    ; TIMER 1 FAILURE, HALT
TIMERTEST_2:
   MOV      AL,BL                         ; SET TIMER 1 COUNT
   SUB      CX,CX
   OUT      TIMER+1,AL
TIMERTEST_3:
   MOV      AL,40H                        ; LATCH TIMER 1 COUNT
   OUT      TIMER+3,AL
   NOP                                    ; DELAY FOR TIMER
   NOP
   IN       AL,TIMER+1                    ; READ TIMER 1 COUNT
   AND      BL,AL
   JZ       TIMERTEST_4                   ; CONTINUE
   LOOP     TIMERTEST_3
   HLT                                    ; TIMER FAILURE, HALT
   ;-----------------------------------------------------------------------;
   ; INITIALIZE TIMER 1 TO REFRESH MEMORY                                  ;
   ;-----------------------------------------------------------------------;
TIMERTEST_4:
   MOV      AL,18                         ; SET UP DIVISOR FOR REFRESH
   OUT      TIMER+1,AL                    ; WRITE TIMER 1 COUNT REGISTER
   OUT      DMA+0DH,AL                    ; SEND MASTER CLEAR TO DMA 
   ;-----------------------------------------------------------------------;
   ; WRAP DMA CHANNELS ADDR AND COUNT REGISTERS                            ;
   ;-----------------------------------------------------------------------;
   MOV      AL,0FFH                       ; WRITE FFH TO ALL REGISTERS
DMATEST_2:
   MOV      BL,AL                         ; SAVE PATTERN
   MOV      BH,AL
   MOV      CX,08H                        ; SET LOOP COUNTER
   SUB      DX,DX                         ; SET UP REGISTER I/O PORT (0000H)
DMATEST_3:
   OUT      DX,AL                         ; WRITE PATTERN TO REGISTER, LSB
   PUSH     AX
   OUT      DX,AL                         ; MSB OF 16-BIT REGISTER
   MOV      AX,0101H                      ; CLOBBER AX
   IN       AL,DX                         ; READ LSB OF 16-BIT DMA REGISTER
   MOV      AH,AL                         ; SAVE LSB
   IN       AL,DX                         ; READ MSB OF 16-BIT DMA REGISTER
   CMP      BX,AX                         ; WRITE/READ MATCH?
   JE       DMATEST_4                     ; YES, CHECK NEXT REGISTER
   HLT                                    ; NO, HALT
DMATEST_4:
   INC      DX                            ; SET I/O PORT TO NEXT REGISTER
   LOOP     DMATEST_3                     ; WRITE PATTERN
   INC      AL                            ; SET PATTERN TO 0
   JZ       DMATEST_2                     ; WRITE TO CHANNEL REGISTERS 
   ;-----------------------------------------------------------------------;
   ; INITIALIZE AND START DMA CONTROLLER FOR MEMORY REFRESH                ;
   ;-----------------------------------------------------------------------;
   MOV      DS,BX                         ; LOAD ABS0 INTO DS AND ES
   MOV      ES,BX
   MOV      AL,0FFH                       ; SET COUNT OF 64K FOR DRAM REFRESH
   OUT      DMA+1,AL
   PUSH     AX
   OUT      DMA+1,AL
   MOV      DL,0BH                        ; (DX)=000B
   MOV      AL,058H                       ; SET DMA MODE (CH0, READ, AUTOINT)
   OUT      DX,AL                         ; WRITE DMA MODE REGISTER
   MOV      AL,00H                        ; ENABLE DMA CONTROLLER
   OUT      DMA+8,AL                      ; SET UP DMA COMMAND REGISTER
   PUSH     AX
   OUT      DMA+10,AL                     ; ENABLE CHANNEL 0 FOR REFRESH
   MOV      CL,03H
   MOV      AL,41H                        ; SET MODE FOR CHANNEL 1
DMATEST_5:
   OUT      DX,AL
   INC      AL                            ; POINT TO NEXT CHANNEL
   LOOP     DMATEST_5

   ;-----------------------------------------------------------------------;
   ; STORAGE TESTS (16K READ/WRITE).                                       ;
   ; WRITE 0FFH, 055H, 0AAH, 001H, 000H TO FIRST 16K OF STORAGE.           ;
   ; VALIDATE STORAGE ADDRESSABILITY.                                      ;
   ;-----------------------------------------------------------------------;
   MOV      DX,0213H                      ; ENABLE EXPANSION BOX
   MOV      AL,01H
   OUT      DX,AL
   PUSH     ES                            ; PRESERVE ES
   PUSH     AX                            ; PRESERVE AX
   MOV      AX,BDA_SEG                    ; LOAD BDA SEGMENT
   MOV      ES,AX
   POP      AX                            ; RESTORE AX
   MOV      BP,WORD[ES:BDA.RESET_FLAG]    ; LOAD RESET_FLAG VALUE
   POP      ES                            ; RESTORE ES
   CMP      BP,1234H                      ; WARM BOOT?
   JE       STORAGETEST_2                 ; BYPASS STORAGE TESTS
   MOV      SP,[STORAGETEST_1]
   JMP      STGTST                        ; CALL STGTST ROUTINE
STORAGETEST_1:
   JE       STORAGETEST_2                 ; STGTST OK? PROCEED
   HLT                                    ; ERR? HALT
STORAGETEST_2:
   SUB      DI,DI                         ; CLEAR DI
   IN       AL,PPI_PORT_A                 ; DETERMINE BASE RAM SIZE
   AND      AL,0CH                        ; ISOLATE RAM SIZE SWS
   ADD      AL,04H                        ; CALCULATE MEMORY SIZE
   MOV      CL,12
   SHL      AX,CL
   MOV      CX,AX
   CLD                                    ; SET DIR FLAG TO INCREASE
STORAGETEST_3:
   STOSB                                  ; FILL BASE RAM WITH DATA
   LOOP     STORAGETEST_3                 ; LOOP UNTIL ZERO
   PUSH     ES                            ; PRESERVE ES
   PUSH     AX                            ; PRESERVE AX
   MOV      AX,BDA_SEG                    ; LOAD BDA SEGMENT
   MOV      ES,AX
   POP      AX                            ; RESTORE AX
   MOV      WORD[ES:BDA.RESET_FLAG],BP    ; STORE BP BACK IN RESET_FLAG
   POP      ES                            ; RESTORE ES 
   ;-----------------------------------------------------------------------;
   ; DETERMINE I/O CHANNEL RAM SIZE                                        ;
   ;-----------------------------------------------------------------------;
   MOV      AL,0F8H                       ; ENABLE SWITCH 5
   OUT      PPI_PORT_B,AL
   IN       AL,PPI_PORT_C                 ; READ SWITCHES
   AND      AL,00000001B                  ; ISOLATE SWITCH 5
   MOV      CL,12D
   ROL      AX,CL
   MOV      AL,0FCH                       ; DISABLE SW. 5
   OUT      PPI_PORT_B,AL
   IN       AL,PPI_PORT_C
   AND      AL,0FH
   OR       AL,AH                         ; COMBINE SWITCH VALUE
   MOV      BL,AL                         ; SAVE SWITCH VALUE
   MOV      AH,32
   MUL      AH                            ; CALCULATE LENGTH
   PUSH     ES                            ; PRESERVE ES
   PUSH     AX                            ; PRESERVE AX
   MOV      AX,BDA_SEG                    ; LOAD BDA SEGMENT
   MOV      ES,AX
   POP      AX                            ; RESTORE AX
   MOV      WORD[ES:BDA.IO_RAM_SIZE],AX   ; SAVE I/O RAM SIZE
   POP      ES                            ; RESTORE ES
   JZ       STORAGETEST_5
   MOV      DX,1000H                      ; SEGMENT FOR I/O RAM
   MOV      AH,AL
   MOV      AL,0
STORAGETEST_4:
   MOV      ES,DX                         ; LOAD I/O RAM SEGMENT
   MOV      CX,8000H                      ; FILL 32K BYTES
   SUB      DI,DI                         ; CLEAR DI
   REP      STOSB
   ADD      DX,800H                       ; NEXT SEGMENT VALUE
   DEC      BL
   JNZ      STORAGETEST_4
   ;-----------------------------------------------------------------------;
   ; INITIALIZE INTEL 8259 INTERRUPT CONTROLLER                            ;
   ;-----------------------------------------------------------------------;
STORAGETEST_5:
   MOV      AL,13H                        ; ICW1 - EDGE, SNGL, ICW4
   OUT      PIC_CMD,AL
   MOV      AL,08H                        ; SET UP ICW2 - INT TYPE 8 (8 - F)
   OUT      PIC_DATA,AL
   MOV      AL,09H                        ; SET UP ICW4 - BUFFERED, 8086 MODE
   OUT      PIC_DATA,AL
   SUB      AX,AX                         ; POINT ES TO START OF R/W STORAGE
   MOV      ES,AX 
   ;-----------------------------------------------------------------------;
   ; SET UP STACK SEGMENT AND STACK POINTER (SP)                           ;
   ;-----------------------------------------------------------------------;
   MOV      AX,STACK_SEG                  ; LOAD STACK SEGMENT ADDRESS
   MOV      SS,AX                         ; LOAD SS WITH STACK SEGMENT
   MOV      SP,256                        ; STACK POINTER TO TOP OF STACL
   CMP      BP,1234H                      ; RESET FLAG SET?
   JE       STACKTEST_2                   ; YES? SKIP MFG TESTS
   SUB      DI,DI                         ; CLEAR DI
   MOV      DS,DI                         ; CLEAR DS
   MOV      BX,24H
   MOV      WORD[BX],TEMP_INT             ; SET UP KB INTERRUPT
   INC      BX
   INC      BX
   MOV      [BX],CS
   CALL     KBD_RESET                     ; READ KB RESET CODE INTO BL
   CMP      BL,065H                       ; MANUFACTURING TST 2?
   JNZ      STACKTEST_2                   ; JUMP IF NOT
   MOV      DL,255                        ; READ IN TST PROGRAM
STACKTEST_1:
   CALL     SP_TEST
   MOV      AL,BL
   STOSB
   DEC      DL
   JNZ      STACKTEST_1                   ; JUMP IF NOT DONE YET
   INT      3EH                           ; INTERRUPT TYPE 62 ADDR F8H
STACKTEST_2:
   ;-----------------------------------------------------------------------;
   ; SET UP BIOS INTERRUPT VECTOR TABLE                                    ;
   ;-----------------------------------------------------------------------;
   CALL     SETUP_IVT                     ; SET UP IVT
   ;-----------------------------------------------------------------------;
   ; INTEL 8259 INTERRUPT CONTROLLER TESTS                                 ;
   ; READ/WRITE IMR WITH ALL 1s and 0s.                                    ;
   ; ENABLE SYSTEM INTERRUPTS.                                             ;
   ; MASK DEVICE INTERRUPTS OFF.                                           ;
   ; CHECK FOR UNEXPECTED HOT INTERRUPTS.                                  ;
   ;-----------------------------------------------------------------------;
   MOV      DX,0021H                      ; INTERRUPT CHIP ADDR 21H
   MOV      AL,0                          ; SET IMR TO ZERO
   OUT      DX,AL
   IN       AL,DX                         ; READ IMR
   OR       AL,AL                         ; IMR=0?
   JNZ      INTTEST_3                     ; NO? ERR ROUTINE
   MOV      AL,0FFH                       ; DISABLE DEVICE INTERRUPTS
   OUT      DX,AL
   IN       AL,DX                         ; READ IMR
   ADD      AL,1                          ; ALL IMR BITS SET?
   JNZ      INTTEST_3                     ; NO? ERR ROUTINE
   XOR      AH,AH                         ; CLEAR AH
   STI                                    ; ENABLE EXTERNAL INTERRUPTS
   SUB      CX,CX                         ; WAIT 1SEC FOR ANY INTS
INTTEST_1:
   LOOP     INTTEST_1
INTTEST_2:
   LOOP     INTTEST_2
   OR       AH,AH                         ; DID ANY INTS OCCUR?
   JZ       TIMERTEST_5                   ; NO? PROCEED
INTTEST_3:
   MOV      DX,101H                       ; BEEP IF ERR
   CALL     ERR_BEEP                      ; CALL BEEP SUBROUTINE
   CLI                                    ; DISABLE INTERRUPTS
   HLT                                    ; HALT

   ;-----------------------------------------------------------------------;
   ; INTEL 8253 TIMER CHECKOUT                                             ;
   ; VERIFY THAT THE SYSTEM TIMER (0) DOESN'T COUNT TOO FAST OR SLOW.      ;
   ;-----------------------------------------------------------------------;
TIMERTEST_5:
   MOV      AL,0FEH                       ; MASK ALL INTS EXCEPT LVL 0
   OUT      DX,AL                         ; WRITE TO 8253 IMR

   CALL     YHI_CHECK_BOOT_KEYS           ; CHECK FOR SECUREBOOT / NETBOOT
   TEST     AL,AL                         ; (AL)=0 - NO SEQUENCE
   JE       NO_SEQUENCE
   DEC      AL                            ; (AL)=1 - SECUREBOOT SEQUENCE
   JE       SECUREBOOT_SEQUENCE
   DEC      AL                            ; (AL)=2 - NETBOOT SEQUENCE
   JE       NETBOOT_SEQUENCE
   JMP      NO_SEQUENCE
SECUREBOOT_SEQUENCE:
   ;CALL     SECUREBOOT_INTERFACE
   JMP      NO_SEQUENCE
NETBOOT_SEQUENCE:
   ;CALL     NETBOOT_INTERFACE
   JMP      NO_SEQUENCE
NO_SEQUENCE:
   JMP      NO_SEQUENCE
ENDP START

   ;----- INT 11 ----------------------------------------------------------;
   ; EQUIPMENT ENUMERATION                                                 ;
   ;     THIS INTERRUPT ATTEMPTS TO DETERMINE WHICH OPTIONAL DEVICES ARE   ;
   ;     CONNECTED TO THE SYSTEM.                                          ;
   ; INPUT                                                                 ;
   ;     NO REGISTERS                                                      ;
   ;     DURING POST, THE EQUIPMENT_FLAG VARIABLE IS SET.                  ;
   ; OUTPUT                                                                ;
   ;     (AX) IS SET ACCORDING TO CONNECTED OPTIONAL DEVICES.              ;
   ;     BIT SET - DEVICE ATTACHED.                                        ;
   ;-----------------------------------------------------------------------;
PROC INT_11
   STI                                    ; ENABLE INTERRUPTS
ENDP INT_11

   ;----- INT 15 ----------------------------------------------------------;
   ; CASSETTE I/O                                                          ;
   ;     THIS INTERRUPT PROVIDES CASSETTE SUPPORT                          ;
   ; INPUT                                                                 ;
   ;     (AH)=0   TURN CASSETTE MOTOR ON                                   ;
   ;     (AH)=1   TURN CASSETTE MOTOR OFF                                  ;
   ;     (AH)=2   READ 256 BYTE BLOCK(S) FROM CASSETTE                     ;
   ;              (ES,BX) = POINTER TO DATA BUFFER                         ;
   ;              (CX) = NUMBER OF BYTES TO READ                           ;
   ;     
   ;-----------------------------------------------------------------------;
PROC INT_15
   STI                                    ; ENABLE INTERRUPTS
   PUSH     DS                            ; SAVE DS
   OR       AH,AH
   JZ       INT_15_1                      ; (AH)=0 - MOTOR ON
   DEC      AH
   JZ       INT_15_2                      ; (AH)=1 - MOTOR OFF
   DEC      AH
   JZ       INT_15_3                      ; (AH)=2 - BLOCK READ
   JMP      INT_15_END
   ;-----------------------------------------------------------------------;    
   ;                           (AH)=0 - MOTOR ON                           ;                              
   ;-----------------------------------------------------------------------; 
INT_15_1:
   IN       AL,PPI_PORT_B                 ; READ PORT 0x61 VALUE
   AND      AL,0F7H                       ; CLEAR BIT 3 TO TURN ON MOTOR
   OUT      PPI_PORT_B,AL                 ; WRITE BACK TO PORT 0x61
   JMP      INT_15_END                    ; RETURN FROM INTERRUPT
   ;-----------------------------------------------------------------------;    
   ;                           (AH)=1 - MOTOR OFF                          ;                             
   ;-----------------------------------------------------------------------;
INT_15_2:
   IN       AL,PPI_PORT_B                 ; READ PORT 0x61 VALUE
   OR       AL,08H                        ; SET BIT 3 TO TURN OFF MOTOR
   OUT      PPI_PORT_B,AL                 ; WRITE BACK TO PORT 0x61
   JMP      INT_15_END                    ; RETURN FROM INTERRUPT
   ;-----------------------------------------------------------------------;    
   ;                          (AH)=2 - BLOCK READ                          ;                             
   ;-----------------------------------------------------------------------;
INT_15_3:
INT_15_END:
   POP      DS                            ; RESTORE DS
   IRET                                   ; RETURN TO CALLER
ENDP INT_15

   ;----- INT 16 ----------------------------------------------------------;
   ; KEYBOARD I/O                                                          ;
   ;     THIS INTERRUPT PROVIDES KEYBOARD SUPPORT                          ;
   ; INPUT                                                                 ;
   ;     (AH)=0   READ NEXT ASCII CHARACTER FROM                           ;
   ;              THE KEYBOARD. RETURN RESULT IN                           ;
   ;              (AL), SCAN CODE IN (AH).                                 ;
   ;     (AH)=1   SET THE Z FLAG TO INDICATE IF AN ASCII CHARACTER IS      ;
   ;              AVAILABLE FOR READING.                                   ;
   ;              (ZF)=1 -- NO CHAR AVAILABLE                              ;
   ;              (ZF)=0 -- CHAR AVAILABLE                                 ;
   ;              IF ZF=0, NEXT CHAR IN THE BUFFER TO BE READ IS IN AX.    ;
   ;     (AH)=2   RETURN CURRENT SHIFT STATUS IN THE AL REGISTER.          ;
   ; OUTPUT                                                                ;
   ;     ALL REGISTERS PRESERVED, EXCEPT FOR AX AND FLAGS                  ;
   ;-----------------------------------------------------------------------;
PROC INT_16
   STI                                    ; ENABLE INTERRUPTS
   PUSH     DS                            ; SAVE DS
   PUSH     BX                            ; SAVE BX
   OR       AH,AH
   JZ       INT_16_1                      ; (AH)=0 - ASCII READ
   DEC      AH
   JZ       INT_16_2                      ; (AH)=1 - ASCII STATUS
   DEC      AH
   JZ       INT_16_3                      ; (AH)=2 - SHIFT STATUS
   JMP      INT_16_END
   ;-----------------------------------------------------------------------;    
   ;                          (AH)=0 - ASCII READ                          ;                             
   ;-----------------------------------------------------------------------;
INT_16_1:
   STI                                    ; ENABLE INTERRUPTS
   NOP                                    ; ALLOW INTERRUPT TO OCCUR
   CLI                                    ; DISABLE INTERRUPTS
   ;MOV   BX,BUFFER_HEAD                  ; POINTER TO HEAD OF BUFFER
   ;CMP   BX,BUFFER_TAIL                  ; TEST END OF BUFFER
   JZ       INT_16_1                      ; LOOP UNTIL BUFFER ISN'T EMPTY
   MOV      AX,[BX]                       ; GET SCAN AND ASCII CODE
   ;MOV   BUFFER_HEAD,BX                  ; STORE VALUE
   JMP      INT_16_END                    ; RETURN FROM INTERRUPT
   ;-----------------------------------------------------------------------;    
   ;                         (AH)=1 - ASCII STATUS                         ;                            
   ;-----------------------------------------------------------------------;
INT_16_2:
   CLI                                    ; DISABLE INTERRUPTS
   ; MOV BX,BUFFER_HEAD                   ; POINTER TO HEAD OF BUFFER
   ; CMP BX,BUFFER_TAIL                   ; TEST END OF BUFFER
   MOV      AX,[BX]                       ; LOAD NEXT CHAR INTO AX (Z=0)
   STI                                    ; ENABLE INTERRUPTS
   JMP      INT_16_END                    ; RETURN FROM INTERRUPT
   ;-----------------------------------------------------------------------;    
   ;                         (AH)=2 - SHIFT STATUS                         ;                            
   ;-----------------------------------------------------------------------;
INT_16_3:
   MOV      AL,KBD_FLAG_0                 ; LOAD SHIFT STATUS FLAG
INT_16_END:
   POP      BX                            ; RECOVER BX
   POP      DS                            ; RECOVER DS
   IRET                                   ; RETURN TO CALLER
ENDP INT_16

   ;----- INT 17 ----------------------------------------------------------;
   ; PRINTER I/O                                                           ;
   ;     THIS INTERRUPT PROVIDES PRINTER SUPPORT                           ;
   ; INPUT                                                                 ;
   ;     (AH)=0   PRINT THE CHARACTER IN (AL)                              ;
   ;              ON RETURN, AH=1 IF CHARACTER COULD NOT BE PRINTED (TO).  ;
   ;              ALL OTHER BITS SET AS ON NORMAL STATUS CALL.             ;
   ;     (AH)=1   INITIALIZE THE PRINTER PORT.                             ;
   ;              RETURNS (AH) SET TO PRINTER STATUS.                      ;
   ;     (AH)=2   READ PRINTER STATUS INTO (AH)                            ;
   ;     (DX)     PRINTER TO BE USED (0,1,2)                               ;
   ; OUTPUT                                                                ;
   ;     ALL REGISTERS PRESERVED, EXCEPT FOR AH                            ;
   ; NOTES                                                                 ;
   ;     PRINTER STATUS                                                    ;
   ;     7     6     5     4     3     2:1   0                             ;
   ;     |     |     |     |     |     |     {_ 1 = TIME OUT               ;
   ;     |     |     |     |     |     {_ UNUSED                           ;
   ;     |     |     |     |     {_ 1 = I/O ERR                            ;
   ;     |     |     |     {_ 1 = SELECTED                                 ;
   ;     |     |     {_ 1 = OUT OF PAPER                                   ;
   ;     |     {_ 1 = ACKNOWLEDGE                                          ;
   ;     {_ 1 = NOT BUSY                                                   ;
   ;-----------------------------------------------------------------------;
PROC INT_17
   STI                                    ; ENABLE INTERRUPTS
   PUSH     DS
   PUSH     DX
   PUSH     SI
   PUSH     CX
   PUSH     BX

   MOV      SI,DX                         ; LOAD PRINTER INDEX INTO (SI)
   MOV      BL,0
   SHL      SI,1
   MOV      DX,0
   OR       DX,DX
   JZ       INT_17_1
INT_17_1:
   POP      BX
   POP      CX
   POP      SI
   POP      DX
   POP      DS
   IRET
INT_17_2:
   PUSH     AX
   OUT      DX,AL
   INC      DX
INT_17_3:
   SUB      CX,CX
   IRET
ENDP INT_17

   ;----- INT 19 ----------------------------------------------------------;
   ; BOOTSTRAP LOADER                                                      ;
   ;     IF A 5 1/4" DISKETTE DRIVE IS CONNECTED TO THE SYSTEM,            ;
   ;     TRACK 0, SECTOR 1 IS READ INTO THE BOOT LOCATION (SEG 0, 7C00),   ;
   ;     AND CONTROL IS TRANSFERED THERE.                                  ;
   ;                                                                       ;
   ;     IF THERE IS NO DISKETTE DRIVE, OR IF A HARDWARE FAULT OCCURRED,   ;
   ;     CONTROL IS TRANSFERRED TO THE RESIDENT BASIC DISKETTE ENTRYPOINT. ;
   ;                                                                       ;
   ;     IF SECURE BOOT IS ENABLED (YHI_BOOT_SECURE=01H), AND THE BOOT     ;
   ;     SECTOR DOES NOT MATCH ANY OF THE ENROLLED SIGNATURES,             ;
   ;     CONTROL IS TRANSFERRED TO THE RESIDENT BASIC DISKETTE ENTRYPOINT. ;
   ;-----------------------------------------------------------------------;
PROC INT_19
   STI                                    ; ENABLE INTERRUPTS
   SUB      AX,AX
   MOV      DS,AX

   MOV      AL,YHI_BOOT_SECURE
   CMP      AL,01H
   JNE      INT_19_BOOT_STANDARD
INT_19_BOOT_SECURE:

INT_19_BOOT_STANDARD:
ENDP INT_19

   ;----- INT 43 ----------------------------------------------------------;
   ; SECUREBOOT INTERFACE                                                  ;
   ;     THIS INTERRUPT PROVIDES SECUREBOOT FUNCTIONALITY                  ;
   ; INPUT                                                                 ;
   ;     (AH)=0   RETURN SECUREBOOT STATUS IN AL                           ;
   ;     (AH)=1   ENROLL NEW BOOTSECTOR HASH                               ;
   ;     (AH)=2   REMOVE BOOTSECTOR HASH                                   ;
   ;     (AH)=3   LIST ENROLLED HASHES                                     ;
   ;     (AH)=4   ENABLE/DISABLE SECUREBOOT                                ;
   ;-----------------------------------------------------------------------;
PROC INT_43
   STI                                    ; ENABLE INTERRUPTS
ENDP INT_43

   ;-----------------------------------------------------------------------;    
   ;                         POWER-ON RESET VECTOR                         ;                            
   ;-----------------------------------------------------------------------; 
   TIMES 1FF0H-($-$$) DB 0               ; PAD TO F000:FFF0

RESET_VECTOR:
   JMP      BIOS_SEG:START
   DB       '03/09/25'

   ;-----------------------------------------------------------------------;    
   ;                             ROM SIGNATURE                             ;                                
   ;-----------------------------------------------------------------------;
   TIMES 1FFFH-($-$$) DB 0             ; PAD TO F000:FFFF
   DB BIOS_SIGNATURE
