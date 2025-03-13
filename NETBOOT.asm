   ;-----------------------------------------------------------------------;    
   ;                  IBM 5150 NETWORK BOOT EXPANSION ROM                  ;
   ; AUTHOR:   VMX                                                         ;
   ; DATE:     13/03/2025                                                  ;
   ;-----------------------------------------------------------------------;

[BITS 16]

%MACRO PROC 1
   %1:
%ENDMACRO

%MACRO ENDP 1
%ENDMACRO

ROM_HEADER:
   DB       55H, 0AAH                     ; ROM SIGNATURE (AA55H)
   DB       04H                           ; ROM SIZE IN 512-BYTE BLOCKS

PROC INIT_ROUTINE                         ; INITIALIZATION ROUTINE
   PUSH     AX                            ; SAVE VOLATILE REGISTERS
   PUSH     BX
   PUSH     CX
   PUSH     DX
   PUSH     SI
   PUSH     DI
   PUSH     ES
   PUSH     DS
   PUSH     CS                            ; SET UP DATA SEGMENT
   POP      DS
   CALL     INSTALL_INTERRUPT_HANDLERS    ; INSTALL INTERRUPT HANDLERS
   MOV      SI,INIT_SUCCESS_MSG
   CALL     PRINT_STRING
   CLC                                    ; CLEAR CARRY FLAG - SUCCESS
   JMP      INIT_SUCCESS
INIT_FAILED:
   MOV      SI,INIT_FAIL_MSG
   CALL     PRINT_STRING
   STC                                    ; SET CARRY FLAG - FAILURE
INIT_SUCCESS:
   POP      DS
   POP      ES
   POP      DI
   POP      DX
   POP      CX
   POP      BX
   POP      AX
   RETF                                   ; RETURN TO BIOS
ENDP INIT_ROUTINE

   ;-----------------------------------------------------------------------;    
   ;                         SUBROUTINES - UTILITY                         ;                            
   ;-----------------------------------------------------------------------;
PROC INSTALL_INTERRUPT_HANDLERS
   PUSH     ES                            ; SAVE ES
   XOR      AX,AX
   MOV      ES,AX
   MOV      WORD[ES:60H*4], INT_60        ; INT 60 - NETBOOT SERVICES
   MOV      WORD[ES:60H*4+2],CS
   POP      ES                            ; RESTORE ES
   RET                                    ; RETURN TO CALLER
ENDP INSTALL_INTERRUPT_HANDLERS

   ;----- INT 60 ----------------------------------------------------------;
   ; NETBOOT                                                               ;
   ;     THIS INTERRUPT PROVIDES NETWORK BOOT SERVICES                     ;
   ; INPUT                                                                 ;
   ;     (AH)=0   RETURN NETWORK BOOT STATUS IN (AL)                       ;
   ;     (AH)=1   START NETWORK BOOT                                       ;
   ;-----------------------------------------------------------------------;
PROC INT_60
   STI                                    ; ENABLE INTERRUPTS
ENDP INT_60

