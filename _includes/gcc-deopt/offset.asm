; OSX 64-bit, requires special alignment
bits 64
align 16

; For the symbol table
global _offset_imul
global _offset_shift_add

section .text

; Takes x and y, returs x + y*320
_offset_imul:
  imul eax, esi, 320
  add eax, edi
  ret

; The same, but with shifts and adds
_offset_shift_add:
  lea eax, [rsi, rsi*4]
  shl eax, 6
  add eax, edi
  ret
