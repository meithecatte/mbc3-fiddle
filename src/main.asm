INCLUDE "hardware.inc"
INCLUDE "macros.inc"
INCLUDE "structs.inc"

SECTION "DMA Routine", ROM0
OAMDMA:
    ld a, HIGH(wShadowOAM)
    ldh [rDMA], a
    ld a, $28
.wait:
    dec a
    jr nz, .wait
    ret
.end

SECTION "VBlank Variables", HRAM
hVBlankFlag: db
hPressedKeys: db
hHeldKeys: db
hOAMDMA: ds OAMDMA.end - OAMDMA

SECTION "Sprites", WRAM0
    struct Sprite
    bytes 1, YPos
    bytes 1, XPos
    bytes 1, Tile
    bytes 1, Attr
    end_struct

UNION
wShadowOAM: ds $a0
NEXTU
    dstruct Sprite, wMainCursor
    dstruct Sprite, wWriteDigit1
    dstruct Sprite, wWriteDigit2
    dstruct Sprite, wLatchCursor
ENDU

SECTION "OAM Template", ROM0
TemplateOAM:
    db 40,  8, $7f, 0 ; wMainCursor
    db 40, 16, "0", OAMF_PAL1 ; wWriteDigit1
    db 40, 24, "0", 0         ; wWriteDigit2
    db 56,  0, "0", OAMF_PAL1 ; wLatchCursor
.end:

SECTION "VBlank", ROM0[$40]
VBlank:
    push af
    push bc
    push de
    push hl

    ld hl, $9841
    ld e, $0c
.readLoop:
    ld a, e
    ld [$4000], a
    ld a, [$a000]
    call PutHexByte
    inc l
    dec e
    bit 3, e
    jr nz, .readLoop ; stop at 7

    call hOAMDMA

    pop hl
    pop de
    pop bc

    ldh a, [hVBlankFlag]
    and a
    jr z, .lagFrame
    ; we're in WaitVBlank, so it's fine to clobber any registers we want
    xor a
    ldh [hVBlankFlag], a

    ld c, LOW(rP1)
    ld a, $20 ; Select D-pad
    ldh [c], a
REPT 6
    ldh a, [c]
ENDR
    or $F0 ; Set 4 upper bits (give them consistency)
    ld b, a

    ; Filter impossible D-pad combinations
    and $0C ; Filter only Down and Up
    ld a, b
    jr nz, .notUpAndDown
    or $0C ; If both are pressed, "unpress" them
    ld b, a
.notUpAndDown
    and $03 ; Filter only Left and Right
    jr nz, .notLeftAndRight
    ; If both are pressed, "unpress" them
    inc b
    inc b
    inc b
.notLeftAndRight
    swap b ; Put D-pad buttons in upper nibble

    ld a, $10 ; Select buttons
    ldh [c], a
REPT 6
    ldh a, [c]
ENDR

    or $F0 ; Set 4 upper bits
    xor b ; Mix with D-pad bits, and invert all bits (such that pressed=1) thanks to "or $F0"
    ld b, a

    ; Release joypad
    ld a, $30
    ldh [c], a

    ldh a, [hHeldKeys]
    cpl
    and b
    ldh [hPressedKeys], a
    ld a, b
    ldh [hHeldKeys], a

    pop af ; Pop off return address as well to exit infinite loop
.lagFrame:
    pop af
    reti

SECTION "WaitVBlank", ROM0
WaitVBlank:
    ld a, 1
    ldh [hVBlankFlag], a
    ; the vblank handler exits us out of this loop
.wait:
    halt
    jr .wait

SECTION "Entrypoint", ROM0[$100]
    jp Start

SECTION "Header", ROM0[$104]
    ds $150 - $104

SECTION "Start", ROM0
Start:
.disableLCD:
    ldh a, [rLY]
    cp SCRN_Y
    jr c, .disableLCD

    xor a
    ldh [rLCDC], a
    ldh [rSCX], a
    ldh [rSCY], a
    ld a, $e0
    ldh [rWY], a
    ldh [rBGP], a
    ldh [rOBP0], a
    ld a, $e4
    ldh [rOBP1], a

    ld hl, OAMDMA
    lb bc, OAMDMA.end - OAMDMA, LOW(hOAMDMA)
.copyOAMDMA:
    ld a, [hli]
    ld [c], a
    inc c
    dec b
    jr nz, .copyOAMDMA

    ld hl, wShadowOAM
    ld de, TemplateOAM
    ld b, TemplateOAM.end - TemplateOAM
.copyOAM:
    ld a, [de]
    inc de
    ld [hli], a
    dec b
    jr nz, .copyOAM

    ld b, $a0 - (TemplateOAM.end - TemplateOAM)
    xor a
.clearOAM:
    ld [hli], a
    dec b
    jr nz, .clearOAM

    call hOAMDMA ; silence BGB warning

    ld hl, vFont
    ld de, Font
    ld bc, Font.end - Font
.fontLoop:
    ld a, [de]
    ld [hl], $ff
    inc l
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, .fontLoop

    ld hl, $9800
    xor a
.tilemapClear:
    ld [hli], a
    bit 2, h ; end at $9c00
    jr z, .tilemapClear

    ld hl, $9801
    ld de, HeaderString
    call PlaceString

    ld l, $a1
    ld de, LatchString
    call PlaceString

    ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON | LCDCF_BG8000
    ldh [rLCDC], a

    ld a, $0a
    ld [$0000], a

    ld a, IEF_VBLANK
    ldh [rIE], a
    xor a
    ldh [hVBlankFlag], a
    ldh [hHeldKeys], a
    ldh [rIF], a
    ei

    ld [wCursorPos], a
    ld [wWriteCursorPos], a
    ld [wWriteValue], a

NUM_OPTIONS EQU 2

MainLoop:
    call WaitVBlank
    ldh a, [hPressedKeys]
    bit PADB_SELECT, a
    jr z, .notSelect

    ld hl, wCursorPos
    inc [hl]
    ld a, [hl]
    cp NUM_OPTIONS
    jr nz, .newCursorPos
    xor a
    ld [hl], a
.newCursorPos
    swap a
    add 40
    ld [wMainCursor_YPos], a
    jr MainLoop

.notSelect:
    and a
    jr z, MainLoop

    ld a, [wCursorPos]
    and a
    jr z, HandleWrite
    ; fallthrough

HandleLatch:
    ldh a, [hPressedKeys]
    bit PADB_LEFT, a
    jr nz, .doLeft
    bit PADB_RIGHT, a
    jr nz, .doRight
    bit PADB_START, a
    jr z, MainLoop
    ; TODO: enable auto

.doLeft:
    ld a, "0"
    ld [wLatchCursor_Tile], a
    ld a, 72
    ld [wLatchCursor_XPos], a
    xor a
    ld [$6000], a
    jr MainLoop

.doRight:
    ld a, "1"
    ld [wLatchCursor_Tile], a
    ld a, 88
    ld [wLatchCursor_XPos], a
    ld a, 1
    ld [$6000], a
    jr MainLoop

NUM_WRITE_SETTINGS EQU 10

HandleWrite:
    ld hl, wWriteCursorPos
    ldh a, [hPressedKeys]
    bit PADB_RIGHT, a
    jr z, .notRight

    inc [hl]
    ld a, [hl]
    sub NUM_WRITE_SETTINGS
    jr nz, .updateCursor
    xor a
    ld [hl], a

.updateCursor:
    ld a, [hl]
    srl a
    ld b, a
    add a
    add b
    add a
    add a
    add a
    add 16
    ld [wWriteDigit1_XPos], a
    add 8
    ld [wWriteDigit2_XPos], a
    ld a, [hl]
    rrca
    ld a, OAMF_PAL1
    jr c, .hlDigit2
    ld [wWriteDigit1_Attr], a
    xor a
    ld [wWriteDigit2_Attr], a
    jp MainLoop

.hlDigit2:
    ld [wWriteDigit2_Attr], a
    xor a
    ld [wWriteDigit1_Attr], a
    jp MainLoop

.notRight:
    bit PADB_LEFT, a
    jr z, .notLeft

    dec [hl]
    ld a, [hl]
    inc a
    jr nz, .updateCursor
    ld a, NUM_WRITE_SETTINGS - 1
    ld [hl], a
    jr .updateCursor
.notLeft:
    bit PADB_UP, a
    jr z, .notUp

    ld a, [hl]
    rrca
    ld a, 1
    jr c, .gotIncrement
    ld a, $10
.gotIncrement:
    ld hl, wWriteValue
    add [hl]
    ld [hl], a
    ld c, a
    swap a
    ld hl, wWriteDigit1_Tile
    call PutHexDigit
    ld a, c
    ld l, LOW(wWriteDigit2_Tile)
    call PutHexDigit
    jp MainLoop

.notUp:
    bit PADB_DOWN, a
    jr z, .notDown

    ld a, [hl]
    rrca
    ld a, -1
    jr c, .gotIncrement
    ld a, -$10
    jr .gotIncrement

.notDown:
    bit PADB_A, a
    jp z, MainLoop

    ld a, [hl]
    srl a
    ld b, a
    ld a, $0c
    sub b
    di
    ld [$4000], a
    ld a, [wWriteValue]
    ld [$a000], a
    ei
    jp MainLoop

SECTION "PlaceString", ROM0
PlaceString:
    ld a, [de]
    and a
    ret z
    ld [hli], a
    inc de
    jr PlaceString

SECTION "PutHex", ROM0
; Converts A to hex and stores it at [hl].
; Clobbers BC.
; Output: HL is one byte after the second hexdigit.
PutHexByte:
    ld c, a
    swap a
    call PutHexDigit
    ld a, c
    ; fallthrough

; Converts the lower nibble of A into a hex character, and stores it in [hl].
; The pointer is incremented.
PutHexDigit:
    and $0f
    add "0"
    cp "9" + 1
    jr c, .notLetter
    add "A" - "0" - 10
.notLetter:
    ld [hli], a
    ret

SECTION "Font", ROM0
Font:
    INCBIN "font.1bpp"
.end:

SECTION "vFont", VRAM[$8200]
vFont: ds 48 * 16

SECTION "Strings", ROM0
HeaderString:
    db "MBC3 RTC Fiddle", 0

LatchString:
    db "Latch: 0 1", 0

SECTION "Variables", WRAM0
wCursorPos: db
wWriteValue: db
wWriteCursorPos: db
