   ;-----------------------------------------------------------------------;    
   ;           SECURE IBM 5150 BIOS IMPLEMENTATION - EXPERIMENTAL          ;             
   ;      CHANGE YHI_BOOT_SECURE IN DATA SECTION TO ENABLE SECURE BOOT     ;
   ;                                                                       ;
   ; AUTHOR:   VMX                                                         ;
   ; DATE:     10/03/2025                                                  ;
   ;-----------------------------------------------------------------------;

[BITS 16]

%MACRO PROC 1
   %1:
%ENDMACRO

%MACRO ENDP 1
%ENDMACRO

   ;-----------------------------------------------------------------------;    
   ;                         CONSTANTS AND EQUATES                         ;                            
   ;-----------------------------------------------------------------------;
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

YHI_BOOT_SECURE   EQU 01H
BIOS_SIGNATURE    EQU 0AAH

   ;-----------------------------------------------------------------------;    
   ;                         SUBROUTINES - UTILITY                         ;                            
   ;-----------------------------------------------------------------------;

   ;-----------------------------------------------------------------------;    
   ;                      INTERRUPT VECTOR TABLE SETUP                     ;                        
   ;-----------------------------------------------------------------------;
PROC SETUP_IVT
   XOR      AX,AX
   MOV      ES,AX

   ; INT 10H - VIDEO SERVICES
   ;MOV      WORD[ES:10H*4], INT_10
   MOV      WORD[ES:10H*4+2], 0F000H
   ; INT 13H - DISK SERVICES
   ;MOV      WORD[ES:13H*4], INT_13
   MOV      WORD[ES:13H*4+2], 0F000H
   ; INT 16H - KEYBOARD SERVICES
   MOV      WORD[ES:16H*4], INT_16
   MOV      WORD[ES:16H*4+2], 0F000H
   ; INT 18H - ROM BASIC
   ;MOV      WORD[ES:18H*4], INT_18
   MOV      WORD[ES:18H*4+2], 0F000H
   ; INT 19H - BOOTSTRAP LOADER
   MOV      WORD[ES:19H*4], INT_19
   MOV      WORD[ES:19H*4+2], 0F000H
ENDP SETUP_IVT

   ;-----------------------------------------------------------------------;    
   ;                       SUBROUTINES - CRYPTOGRAPHY                      ;                         
   ;-----------------------------------------------------------------------;

   ;-----------------------------------------------------------------------;    
   ;                    RESET VECTOR - BIOS ENTRY POINT                    ;                       
   ;-----------------------------------------------------------------------;
PROC START
   CLI                                    ; DISABLE INTERRUPTS
   MOV      AH,0D5H
   SAHF
   JNC      ERR01
   JNZ      ERR01
   JNP      ERR01
   JNS      ERR01
   LAHF
   MOV      CL,5
   SHR      AH,CL
   JNC      ERR01
   MOV      AL,40H
   SHL      AL,1
   JNO      ERR01
   XOR      AH,AH
   SAHF
   JBE      ERR01
   JS       ERR01
   JP       ERR01
   LAHF
   MOV      CL,5
   SHR      AH,CL
   JC       ERR01
   SHL      AH,1

ERR01:
   HLT
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

   ;-----------------------------------------------------------------------;    
   ;                         POWER-ON RESET VECTOR                         ;                            
   ;-----------------------------------------------------------------------; 
   TIMES 01FF0H-($-$$) DB 0 ; PAD TO F000:FFF0

RESET_VECTOR:
   JMP   0F000H:START
   DB    '03/09/25'

   ;-----------------------------------------------------------------------;    
   ;                             ROM SIGNATURE                             ;                                
   ;-----------------------------------------------------------------------;
   TIMES 02000H-1-($-$$) DB 0 ; PAD TO F000:FFFF
   DB BIOS_SIGNATURE

