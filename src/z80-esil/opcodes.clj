(ns z80-esil.opcodes)

(def opcodes
  {nil
   [
    "nop"
    "ld bc, 0x%04x"
    "ld [bc], a"
    "inc bc"
    "inc b"
    "dec b"
    "ld b, 0x%02x"
    "rlca"
    "ex af, af"
    "add hl, bc"
    "ld a, [bc]"
    "dec bc"
    "inc c"
    "dec c"
    "ld c, 0x%02x"
    "rrca"
    "djnz 0x%02x"
    "ld de, 0x%04x"
    "ld [de], a"
    "inc de"
    "inc d"
    "dec d"
    "ld d, 0x%02x"
    "rla"
    "jr 0x%02x"
    "add hl, de"
    "ld a, [de]"
    "dec de"
    "inc e"
    "dec e"
    "ld e, 0x%02x"
    "rra"
    "jr nz, 0x%02x"
    "ld hl, 0x%04x"
    "ld [0x%04x], hl"
    "inc hl"
    "inc h"
    "dec h"
    "ld h, 0x%02x"
    "daa"
    "jr z, 0x%02x"
    "add hl, hl"
    "ld hl, [0x%04x]"
    "dec hl"
    "inc l"
    "dec l"
    "ld l, 0x%02x"
    "cpl"
    "jr nc, 0x%02x"
    "ld sp, 0x%04x"
    "ld [0x%04x], a"
    "inc sp"
    "inc [hl]"
    "dec [hl]"
    "ld [hl], 0x%02x"
    "scf"
    "jr c, 0x%02x"
    "add hl, sp"
    "ld a, [0x%04x]"
    "dec sp"
    "inc a"
    "dec a"
    "ld a, 0x%02x"
    "ccf"
    "ld b, b"
    "ld b, c"
    "ld b, d"
    "ld b, e"
    "ld b, h"
    "ld b, l"
    "ld b, [hl]"
    "ld b, a"
    "ld c, b"
    "ld c, c"
    "ld c, d"
    "ld c, e"
    "ld c, h"
    "ld c, l"
    "ld c, [hl]"
    "ld c, a"
    "ld d, b"
    "ld d, c"
    "ld d, d"
    "ld d, e"
    "ld d, h"
    "ld d, l"
    "ld d, [hl]"
    "ld d, a"
    "ld e, b"
    "ld e, c"
    "ld e, d"
    "ld e, e"
    "ld e, h"
    "ld e, l"
    "ld e, [hl]"
    "ld e, a"
    "ld h, b"
    "ld h, c"
    "ld h, d"
    "ld h, e"
    "ld h, h"
    "ld h, l"
    "ld h, [hl]"
    "ld h, a"
    "ld l, b"
    "ld l, c"
    "ld l, d"
    "ld l, e"
    "ld l, h"
    "ld l, l"
    "ld l, [hl]"
    "ld l, a"
    "ld [hl], b"
    "ld [hl], c"
    "ld [hl], d"
    "ld [hl], e"
    "ld [hl], h"
    "ld [hl], l"
    "halt"
    "ld [hl], a"
    "ld a, b"
    "ld a, c"
    "ld a, d"
    "ld a, e"
    "ld a, h"
    "ld a, l"
    "ld a, [hl]"
    "ld a, a"
    "add a, b"
    "add a, c"
    "add a, d"
    "add a, e"
    "add a, h"
    "add a, l"
    "add a, [hl]"
    "add a, a"
    "adc a, b"
    "adc a, c"
    "adc a, d"
    "adc a, e"
    "adc a, h"
    "adc a, l"
    "adc a, [hl]"
    "adc a, a"
    "sub b"
    "sub c"
    "sub d"
    "sub e"
    "sub h"
    "sub l"
    "sub [hl]"
    "sub a"
    "sbc a, b"
    "sbc a, c"
    "sbc a, d"
    "sbc a, e"
    "sbc a, h"
    "sbc a, l"
    "sbc a, [hl]"
    "sbc a, a"
    "and b"
    "and c"
    "and d"
    "and e"
    "and h"
    "and l"
    "and [hl]"
    "and a"
    "xor b"
    "xor c"
    "xor d"
    "xor e"
    "xor h"
    "xor l"
    "xor [hl]"
    "xor a"
    "or b"
    "or c"
    "or d"
    "or e"
    "or h"
    "or l"
    "or [hl]"
    "or a"
    "cp b"
    "cp c"
    "cp d"
    "cp e"
    "cp h"
    "cp l"
    "cp [hl]"
    "cp a"
    "ret nz"
    "pop bc"
    "jp nz, 0x%04x"
    "jp 0x%04x"
    "call nz, 0x%04x"
    "push bc"
    "add a, 0x%02x"
    "rst 0x00"
    "ret z"
    "ret"
    "jp z, 0x%04x"
    ""
    "call z, 0x%04x"
    "call 0x%04x"
    "adc a, 0x%02x"
    "rst 0x08"
    "ret nc"
    "pop de"
    "jp nc, 0x%04x"
    "out [0x%02x], a"
    "call nc, 0x%04x"
    "push de"
    "sub 0x%02x"
    "rst 0x10"
    "ret c"
    "exx"
    "jp c, 0x%04x"
    "in a, 0x%02x"
    "call c, 0x%04x"
    ""
    "sbc a, 0x%02x"
    "rst 0x18"
    "ret po"
    "pop hl"
    "jp po, 0x%04x"
    "ex [sp], hl"
    "call po, 0x%04x"
    "push hl"
    "and 0x%02x"
    "rst 0x20"
    "ret pe"
    "jp [hl]"
    "jp pe, 0x%04x"
    "ex de, hl"
    "call pe, 0x%04x"
    "!unk"
    "xor 0x%02x"
    "rst 0x28"
    "ret p"
    "pop af"
    "jp p, 0x%04x"
    "di"
    "call p, 0x%04x"
    "push af"
    "or 0x%02x"
    "rst 0x30"
    "ret m"
    "ld sp, hl"
    "jp m, 0x%04x"
    "ei"
    "call m, 0x%04x"
    "!unk"
    "cp 0x%02x"
    "rst 0x38"
    ],
   :dd
   [
    "add ix, bc"
    "add ix, de"
    "ld ix, 0x%04x"
    "ld [0x%04x], ix"
    "inc ix"
    "inc ixh"
    "dec ixh"
    "ld ixh, 0x%02x"
    "add ix, ix"
    "ld ix, [0x%04x]"
    "dec ix"
    "inc ixl"
    "dec ixl"
    "ld ixl, 0x%02x"
    "inc [ix+0x%02x]"
    "dec [ix+0x%02x]"
    "ld [ix+0x%02x], 0x%02x"
    "add ix, sp"
    "ld b, ixh"
    "ld b, ixl"
    "ld b, [ix+0x%02x]"
    "ld c, ixh"
    "ld c, ixl"
    "ld c, [ix+0x%02x]"
    "ld d, ixh"
    "ld d, ixl"
    "ld d, [ix+0x%02x]"
    "ld e, ixh"
    "ld e, ixl"
    "ld e, [ix+0x%02x]"
    "ld ixh, b"
    "ld ixh, c"
    "ld ixh, d"
    "ld ixh, e"
    "ld ixh, ixh"
    "ld ixh, ixl"
    "ld h, [ix+0x%02x]"
    "ld ixh, a"
    "ld ixl, b"
    "ld ixl, c"
    "ld ixl, d"
    "ld ixl, e"
    "ld ixl, ixh"
    "ld ixl, ixl"
    "ld l, [ix+0x%02x]"
    "ld ixl, a"
    "ld [ix+0x%02x], b"
    "ld [ix+0x%02x], c"
    "ld [ix+0x%02x], d"
    "ld [ix+0x%02x], e"
    "ld [ix+0x%02x], h"
    "ld [ix+0x%02x], l"
    "ld [ix+0x%02x], a"
    "ld a, ixh"
    "ld a, ixl"
    "ld a, [ix+0x%02x]"
    "add a, ixh"
    "add a, ixl"
    "add a, [ix+0x%02x]"
    "adc a, ixh"
    "adc a, ixl"
    "adc a, [ix+0x%02x]"
    "sub a, ixh"
    "sub a, ixl"
    "sub [ix+0x%02x]"
    "sbc a, ixh"
    "sbc a, ixl"
    "sbc a, [ix+0x%02x]"
    "and ixh"
    "and ixl"
    "and [ix+0x%02x]"
    "xor ixh"
    "xor ixl"
    "xor [ix+0x%02x]"
    "or ixh"
    "or ixl"
    "or [ix+0x%02x]"
    "cp ixh"
    "cp ixl"
    "cp [ix+0x%02x]"
    ""
    "pop ix"
    "ex [sp], ix"
    "push ix"
    "jp [ix]"
    "ld sp, ix"
    "invalid"
    ],
   :fd
   [
    "add iy, bc"
    "add iy, de"
    "ld iy, 0x%04x"
    "ld [0x%04x], iy"
    "inc iy"
    "inc iyh"
    "dec iyh"
    "ld iyh, 0x%02x"
    "add iy, iy"
    "ld iy, [0x%04x]"
    "dec iy"
    "inc iyl"
    "dec iyl"
    "ld iyl, 0x%02x"
    "inc [iy+0x%02x]"
    "dec [iy+0x%02x]"
    "ld [iy+0x%02x], 0x%02x"
    "add iy, sp"
    "ld b, iyh"
    "ld b, iyl"
    "ld b, [iy+0x%02x]"
    "ld c, iyh"
    "ld c, iyl"
    "ld c, [iy+0x%02x]"
    "ld d, iyh"
    "ld d, iyl"
    "ld d, [iy+0x%02x]"
    "ld e, iyh"
    "ld e, iyl"
    "ld e, [iy+0x%02x]"
    "ld iyh, b"
    "ld iyh, c"
    "ld iyh, d"
    "ld iyh, e"
    "ld iyh, iyh"
    "ld iyh, iyl"
    "ld h, [iy+0x%02x]"
    "ld iyh, a"
    "ld iyl, b"
    "ld iyl, c"
    "ld iyl, d"
    "ld iyl, e"
    "ld iyl, iyh"
    "ld iyl, iyl"
    "ld l, [iy+0x%02x]"
    "ld iyl, a"
    "ld [iy+0x%02x], b"
    "ld [iy+0x%02x], c"
    "ld [iy+0x%02x], d"
    "ld [iy+0x%02x], e"
    "ld [iy+0x%02x], h"
    "ld [iy+0x%02x], l"
    "ld [iy+0x%02x], a"
    "ld a, iyh"
    "ld a, iyl"
    "ld a, [iy+0x%02x]"
    "add a, iyh"
    "add a, iyl"
    "add a, [iy+0x%02x]"
    "adc a, iyh"
    "adc a, iyl"
    "adc a, [iy+0x%02x]"
    "sub iyh"
    "sub iyl"
    "sub [iy+0x%02x]"
    "sbc a, iyh"
    "sbc a, iyl"
    "sbc a, [iy+0x%02x]"
    "and iyh"
    "and iyl"
    "and [iy+0x%02x]"
    "xor iyh"
    "xor iyl"
    "xor [iy+0x%02x]"
    "or iyh"
    "or iyl"
    "or [iy+0x%02x]"
    "cp iyh"
    "cp iyl"
    "cp [iy+0x%02x]"
    ""
    "pop iy"
    "ex [sp], iy"
    "push iy"
    "jp [iy]"
    "ld sp, iy"
    "invalid"
    ],
   :ed
   [
    "in b, [c]"
    "out [c], b"
    "sbc hl, bc"
    "ld [0x%04x], bc"
    "neg"
    "retn"
    "im 0"
    "ld i, a"
    "in c, [c]"
    "out [c], c"
    "adc hl, bc"
    "ld bc, [0x%04x]"
    "reti"
    "ld r, a"
    "in d, [c]"
    "out [c], d"
    "sbc hl, de"
    "ld [0x%04x], de"
    "im 1"
    "ld a, i"
    "in e, [c]"
    "out [c], e"
    "adc hl, de"
    "ld de, [0x%04x]"
    "im 2"
    "ld a, r"
    "in h, [c]"
    "out [c], h"
    "sbc hl, hl"
    "rrd"
    "in l, [c]"
    "out [c], l"
    "adc hl, hl"
    "rld"
    "in [c]"
    "in f, [c]"
    "out [c], 0"
    "sbc hl, sp"
    "ld [0x%04x], sp"
    "in a, [c]"
    "out [c], a"
    "adc hl, sp"
    "ld sp, [0x%04x]"
    "ldi"
    "cpi"
    "ini"
    "outi"
    "ldd"
    "cpd"
    "ind"
    "outd"
    "ldir"
    "cpir"
    "inir"
    "otir"
    "lddr"
    "cpdr"
    "indr"
    "otdr"
    "invalid"
    ]
   :ddcb
   [
    "ld b, rlc [ix+0x%02x]",
    "ld c, rlc [ix+0x%02x]",
    "ld d, rlc [ix+0x%02x]",
    "ld e, rlc [ix+0x%02x]",
    "ld h, rlc [ix+0x%02x]",
    "ld l, rlc [ix+0x%02x]",
    "rlc [ix+0x%02x]",
    "ld a, rlc [ix+0x%02x]",
    "ld b, rrc [ix+0x%02x]",
    "ld c, rrc [ix+0x%02x]",
    "ld d, rrc [ix+0x%02x]",
    "ld e, rrc [ix+0x%02x]",
    "ld h, rrc [ix+0x%02x]",
    "ld l, rrc [ix+0x%02x]",
    "rrc [ix+0x%02x]",
    "ld a, rrc [ix+0x%02x]",
    "ld b, rl [ix+0x%02x]",
    "ld c, rl [ix+0x%02x]",
    "ld d, rl [ix+0x%02x]",
    "ld e, rl [ix+0x%02x]",
    "ld h, rl [ix+0x%02x]",
    "ld l, rl [ix+0x%02x]",
    "rl [ix+0x%02x]",
    "ld a, rl [ix+0x%02x]",
    "ld b, rr [ix+0x%02x]",
    "ld c, rr [ix+0x%02x]",
    "ld d, rr [ix+0x%02x]",
    "ld e, rr [ix+0x%02x]",
    "ld h, rr [ix+0x%02x]",
    "ld l, rr [ix+0x%02x]",
    "rr [ix+0x%02x]",
    "ld a, rr [ix+0x%02x]",
    "ld b, sla [ix+0x%02x]",
    "ld c, sla [ix+0x%02x]",
    "ld d, sla [ix+0x%02x]",
    "ld e, sla [ix+0x%02x]",
    "ld h, sla [ix+0x%02x]",
    "ld l, sla [ix+0x%02x]",
    "sla [ix+0x%02x]",
    "ld a, sla [ix+0x%02x]",
    "ld b, sra [ix+0x%02x]",
    "ld c, sra [ix+0x%02x]",
    "ld d, sra [ix+0x%02x]",
    "ld e, sra [ix+0x%02x]",
    "ld h, sra [ix+0x%02x]",
    "ld l, sra [ix+0x%02x]",
    "sra [ix+0x%02x]",
    "ld a, sra [ix+0x%02x]",
    "ld b, sll [ix+0x%02x]",
    "ld c, sll [ix+0x%02x]",
    "ld d, sll [ix+0x%02x]",
    "ld e, sll [ix+0x%02x]",
    "ld h, sll [ix+0x%02x]",
    "ld l, sll [ix+0x%02x]",
    "sll [ix+0x%02x]",
    "ld a, sll [ix+0x%02x]",
    "ld b, srl [ix+0x%02x]",
    "ld c, srl [ix+0x%02x]",
    "ld d, srl [ix+0x%02x]",
    "ld e, srl [ix+0x%02x]",
    "ld h, srl [ix+0x%02x]",
    "ld l, srl [ix+0x%02x]",
    "srl [ix+0x%02x]",
    "ld a, srl [ix+0x%02x]",
    "bit 0, [ix+0x%02x]",
    "bit 1, [ix+0x%02x]",
    "bit 2, [ix+0x%02x]",
    "bit 3, [ix+0x%02x]",
    "bit 4, [ix+0x%02x]",
    "bit 5, [ix+0x%02x]",
    "bit 6, [ix+0x%02x]",
    "bit 7, [ix+0x%02x]",
    "ld b, res 0, [ix+0x%02x]",
    "ld c, res 0, [ix+0x%02x]",
    "ld d, res 0, [ix+0x%02x]",
    "ld e, res 0, [ix+0x%02x]",
    "ld h, res 0, [ix+0x%02x]",
    "ld l, res 0, [ix+0x%02x]",
    "res 0, [ix+0x%02x]",
    "ld a, res 0, [ix+0x%02x]",
    "ld b, res 1, [ix+0x%02x]",
    "ld c, res 1, [ix+0x%02x]",
    "ld d, res 1, [ix+0x%02x]",
    "ld e, res 1, [ix+0x%02x]",
    "ld h, res 1, [ix+0x%02x]",
    "ld l, res 1, [ix+0x%02x]",
    "res 1, [ix+0x%02x]",
    "ld a, res 1, [ix+0x%02x]",
    "ld b, res 2, [ix+0x%02x]",
    "ld c, res 2, [ix+0x%02x]",
    "ld d, res 2, [ix+0x%02x]",
    "ld e, res 2, [ix+0x%02x]",
    "ld h, res 2, [ix+0x%02x]",
    "ld l, res 2, [ix+0x%02x]",
    "res 2, [ix+0x%02x]",
    "ld a, res 2, [ix+0x%02x]",
    "ld b, res 3, [ix+0x%02x]",
    "ld c, res 3, [ix+0x%02x]",
    "ld d, res 3, [ix+0x%02x]",
    "ld e, res 3, [ix+0x%02x]",
    "ld h, res 3, [ix+0x%02x]",
    "ld l, res 3, [ix+0x%02x]",
    "res 3, [ix+0x%02x]",
    "ld a, res 3, [ix+0x%02x]",
    "ld b, res 4, [ix+0x%02x]",
    "ld c, res 4, [ix+0x%02x]",
    "ld d, res 4, [ix+0x%02x]",
    "ld e, res 4, [ix+0x%02x]",
    "ld h, res 4, [ix+0x%02x]",
    "ld l, res 4, [ix+0x%02x]",
    "res 4, [ix+0x%02x]",
    "ld a, res 4, [ix+0x%02x]",
    "ld b, res 5, [ix+0x%02x]",
    "ld c, res 5, [ix+0x%02x]",
    "ld d, res 5, [ix+0x%02x]",
    "ld e, res 5, [ix+0x%02x]",
    "ld h, res 5, [ix+0x%02x]",
    "ld l, res 5, [ix+0x%02x]",
    "res 5, [ix+0x%02x]",
    "ld a, res 5, [ix+0x%02x]",
    "ld b, res 6, [ix+0x%02x]",
    "ld c, res 6, [ix+0x%02x]",
    "ld d, res 6, [ix+0x%02x]",
    "ld e, res 6, [ix+0x%02x]",
    "ld h, res 6, [ix+0x%02x]",
    "ld l, res 6, [ix+0x%02x]",
    "res 6, [ix+0x%02x]",
    "ld a, res 6, [ix+0x%02x]",
    "ld b, res 7, [ix+0x%02x]",
    "ld c, res 7, [ix+0x%02x]",
    "ld d, res 7, [ix+0x%02x]",
    "ld e, res 7, [ix+0x%02x]",
    "ld h, res 7, [ix+0x%02x]",
    "ld l, res 7, [ix+0x%02x]",
    "res 7, [ix+0x%02x]",
    "ld a, res 7, [ix+0x%02x]",
    "ld b, set 0, [ix+0x%02x]",
    "ld c, set 0, [ix+0x%02x]",
    "ld d, set 0, [ix+0x%02x]",
    "ld e, set 0, [ix+0x%02x]",
    "ld h, set 0, [ix+0x%02x]",
    "ld l, set 0, [ix+0x%02x]",
    "set 0, [ix+0x%02x]",
    "ld a, set 0, [ix+0x%02x]",
    "ld b, set 1, [ix+0x%02x]",
    "ld c, set 1, [ix+0x%02x]",
    "ld d, set 1, [ix+0x%02x]",
    "ld e, set 1, [ix+0x%02x]",
    "ld h, set 1, [ix+0x%02x]",
    "ld l, set 1, [ix+0x%02x]",
    "set 1, [ix+0x%02x]",
    "ld a, set 1, [ix+0x%02x]",
    "ld b, set 2, [ix+0x%02x]",
    "ld c, set 2, [ix+0x%02x]",
    "ld d, set 2, [ix+0x%02x]",
    "ld e, set 2, [ix+0x%02x]",
    "ld h, set 2, [ix+0x%02x]",
    "ld l, set 2, [ix+0x%02x]",
    "set 2, [ix+0x%02x]",
    "ld a, set 2, [ix+0x%02x]",
    "ld b, set 3, [ix+0x%02x]",
    "ld c, set 3, [ix+0x%02x]",
    "ld d, set 3, [ix+0x%02x]",
    "ld e, set 3, [ix+0x%02x]",
    "ld h, set 3, [ix+0x%02x]",
    "ld l, set 3, [ix+0x%02x]",
    "set 3, [ix+0x%02x]",
    "ld a, set 3, [ix+0x%02x]",
    "ld b, set 4, [ix+0x%02x]",
    "ld c, set 4, [ix+0x%02x]",
    "ld d, set 4, [ix+0x%02x]",
    "ld e, set 4, [ix+0x%02x]",
    "ld h, set 4, [ix+0x%02x]",
    "ld l, set 4, [ix+0x%02x]",
    "set 4, [ix+0x%02x]",
    "ld a, set 4, [ix+0x%02x]",
    "ld b, set 5, [ix+0x%02x]",
    "ld c, set 5, [ix+0x%02x]",
    "ld d, set 5, [ix+0x%02x]",
    "ld e, set 5, [ix+0x%02x]",
    "ld h, set 5, [ix+0x%02x]",
    "ld l, set 5, [ix+0x%02x]",
    "set 5, [ix+0x%02x]",
    "ld a, set 5, [ix+0x%02x]",
    "ld b, set 6, [ix+0x%02x]",
    "ld c, set 6, [ix+0x%02x]",
    "ld d, set 6, [ix+0x%02x]",
    "ld e, set 6, [ix+0x%02x]",
    "ld h, set 6, [ix+0x%02x]",
    "ld l, set 6, [ix+0x%02x]",
    "set 6, [ix+0x%02x]",
    "ld a, set 6, [ix+0x%02x]",
    "ld b, set 7, [ix+0x%02x]",
    "ld c, set 7, [ix+0x%02x]",
    "ld d, set 7, [ix+0x%02x]",
    "ld e, set 7, [ix+0x%02x]",
    "ld h, set 7, [ix+0x%02x]",
    "ld l, set 7, [ix+0x%02x]",
    "set 7, [ix+0x%02x]",
    "ld a, set 7, [ix+0x%02x]",
    "invalid",
    ]
   :fdcb
   [
    "ld b, rlc [iy+0x%02x]",
    "ld c, rlc [iy+0x%02x]",
    "ld d, rlc [iy+0x%02x]",
    "ld e, rlc [iy+0x%02x]",
    "ld h, rlc [iy+0x%02x]",
    "ld l, rlc [iy+0x%02x]",
    "rlc [iy+0x%02x]",
    "ld a, rlc [iy+0x%02x]",
    "ld b, rrc [iy+0x%02x]",
    "ld c, rrc [iy+0x%02x]",
    "ld d, rrc [iy+0x%02x]",
    "ld e, rrc [iy+0x%02x]",
    "ld h, rrc [iy+0x%02x]",
    "ld l, rrc [iy+0x%02x]",
    "rrc [iy+0x%02x]",
    "ld a, rrc [iy+0x%02x]",
    "ld b, rl [iy+0x%02x]",
    "ld c, rl [iy+0x%02x]",
    "ld d, rl [iy+0x%02x]",
    "ld e, rl [iy+0x%02x]",
    "ld h, rl [iy+0x%02x]",
    "ld l, rl [iy+0x%02x]",
    "rl [iy+0x%02x]",
    "ld a, rl [iy+0x%02x]",
    "ld b, rr [iy+0x%02x]",
    "ld c, rr [iy+0x%02x]",
    "ld d, rr [iy+0x%02x]",
    "ld e, rr [iy+0x%02x]",
    "ld h, rr [iy+0x%02x]",
    "ld l, rr [iy+0x%02x]",
    "rr [iy+0x%02x]",
    "ld a, rr [iy+0x%02x]",
    "ld b, sla [iy+0x%02x]",
    "ld c, sla [iy+0x%02x]",
    "ld d, sla [iy+0x%02x]",
    "ld e, sla [iy+0x%02x]",
    "ld h, sla [iy+0x%02x]",
    "ld l, sla [iy+0x%02x]",
    "sla [iy+0x%02x]",
    "ld a, sla [iy+0x%02x]",
    "ld b, sra [iy+0x%02x]",
    "ld c, sra [iy+0x%02x]",
    "ld d, sra [iy+0x%02x]",
    "ld e, sra [iy+0x%02x]",
    "ld h, sra [iy+0x%02x]",
    "ld l, sra [iy+0x%02x]",
    "sra [iy+0x%02x]",
    "ld a, sra [iy+0x%02x]",
    "ld b, sll [iy+0x%02x]",
    "ld c, sll [iy+0x%02x]",
    "ld d, sll [iy+0x%02x]",
    "ld e, sll [iy+0x%02x]",
    "ld h, sll [iy+0x%02x]",
    "ld l, sll [iy+0x%02x]",
    "sll [iy+0x%02x]",
    "ld a, sll [iy+0x%02x]",
    "ld b, srl [iy+0x%02x]",
    "ld c, srl [iy+0x%02x]",
    "ld d, srl [iy+0x%02x]",
    "ld e, srl [iy+0x%02x]",
    "ld h, srl [iy+0x%02x]",
    "ld l, srl [iy+0x%02x]",
    "srl [iy+0x%02x]",
    "ld a, srl [iy+0x%02x]",
    "bit 0, [iy+0x%02x]",
    "bit 1, [iy+0x%02x]",
    "bit 2, [iy+0x%02x]",
    "bit 3, [iy+0x%02x]",
    "bit 4, [iy+0x%02x]",
    "bit 5, [iy+0x%02x]",
    "bit 6, [iy+0x%02x]",
    "bit 7, [iy+0x%02x]",
    "ld b, res 0, [iy+0x%02x]",
    "ld c, res 0, [iy+0x%02x]",
    "ld d, res 0, [iy+0x%02x]",
    "ld e, res 0, [iy+0x%02x]",
    "ld h, res 0, [iy+0x%02x]",
    "ld l, res 0, [iy+0x%02x]",
    "res 0, [iy+0x%02x]",
    "ld a, res 0, [iy+0x%02x]",
    "ld b, res 1, [iy+0x%02x]",
    "ld c, res 1, [iy+0x%02x]",
    "ld d, res 1, [iy+0x%02x]",
    "ld e, res 1, [iy+0x%02x]",
    "ld h, res 1, [iy+0x%02x]",
    "ld l, res 1, [iy+0x%02x]",
    "res 1, [iy+0x%02x]",
    "ld a, res 1, [iy+0x%02x]",
    "ld b, res 2, [iy+0x%02x]",
    "ld c, res 2, [iy+0x%02x]",
    "ld d, res 2, [iy+0x%02x]",
    "ld e, res 2, [iy+0x%02x]",
    "ld h, res 2, [iy+0x%02x]",
    "ld l, res 2, [iy+0x%02x]",
    "res 2, [iy+0x%02x]",
    "ld a, res 2, [iy+0x%02x]",
    "ld b, res 3, [iy+0x%02x]",
    "ld c, res 3, [iy+0x%02x]",
    "ld d, res 3, [iy+0x%02x]",
    "ld e, res 3, [iy+0x%02x]",
    "ld h, res 3, [iy+0x%02x]",
    "ld l, res 3, [iy+0x%02x]",
    "res 3, [iy+0x%02x]",
    "ld a, res 3, [iy+0x%02x]",
    "ld b, res 4, [iy+0x%02x]",
    "ld c, res 4, [iy+0x%02x]",
    "ld d, res 4, [iy+0x%02x]",
    "ld e, res 4, [iy+0x%02x]",
    "ld h, res 4, [iy+0x%02x]",
    "ld l, res 4, [iy+0x%02x]",
    "res 4, [iy+0x%02x]",
    "ld a, res 4, [iy+0x%02x]",
    "ld b, res 5, [iy+0x%02x]",
    "ld c, res 5, [iy+0x%02x]",
    "ld d, res 5, [iy+0x%02x]",
    "ld e, res 5, [iy+0x%02x]",
    "ld h, res 5, [iy+0x%02x]",
    "ld l, res 5, [iy+0x%02x]",
    "res 5, [iy+0x%02x]",
    "ld a, res 5, [iy+0x%02x]",
    "ld b, res 6, [iy+0x%02x]",
    "ld c, res 6, [iy+0x%02x]",
    "ld d, res 6, [iy+0x%02x]",
    "ld e, res 6, [iy+0x%02x]",
    "ld h, res 6, [iy+0x%02x]",
    "ld l, res 6, [iy+0x%02x]",
    "res 6, [iy+0x%02x]",
    "ld a, res 6, [iy+0x%02x]",
    "ld b, res 7, [iy+0x%02x]",
    "ld c, res 7, [iy+0x%02x]",
    "ld d, res 7, [iy+0x%02x]",
    "ld e, res 7, [iy+0x%02x]",
    "ld h, res 7, [iy+0x%02x]",
    "ld l, res 7, [iy+0x%02x]",
    "res 7, [iy+0x%02x]",
    "ld a, res 7, [iy+0x%02x]",
    "ld b, set 0, [iy+0x%02x]",
    "ld c, set 0, [iy+0x%02x]",
    "ld d, set 0, [iy+0x%02x]",
    "ld e, set 0, [iy+0x%02x]",
    "ld h, set 0, [iy+0x%02x]",
    "ld l, set 0, [iy+0x%02x]",
    "set 0, [iy+0x%02x]",
    "ld a, set 0, [iy+0x%02x]",
    "ld b, set 1, [iy+0x%02x]",
    "ld c, set 1, [iy+0x%02x]",
    "ld d, set 1, [iy+0x%02x]",
    "ld e, set 1, [iy+0x%02x]",
    "ld h, set 1, [iy+0x%02x]",
    "ld l, set 1, [iy+0x%02x]",
    "set 1, [iy+0x%02x]",
    "ld a, set 1, [iy+0x%02x]",
    "ld b, set 2, [iy+0x%02x]",
    "ld c, set 2, [iy+0x%02x]",
    "ld d, set 2, [iy+0x%02x]",
    "ld e, set 2, [iy+0x%02x]",
    "ld h, set 2, [iy+0x%02x]",
    "ld l, set 2, [iy+0x%02x]",
    "set 2, [iy+0x%02x]",
    "ld a, set 2, [iy+0x%02x]",
    "ld b, set 3, [iy+0x%02x]",
    "ld c, set 3, [iy+0x%02x]",
    "ld d, set 3, [iy+0x%02x]",
    "ld e, set 3, [iy+0x%02x]",
    "ld h, set 3, [iy+0x%02x]",
    "ld l, set 3, [iy+0x%02x]",
    "set 3, [iy+0x%02x]",
    "ld a, set 3, [iy+0x%02x]",
    "ld b, set 4, [iy+0x%02x]",
    "ld c, set 4, [iy+0x%02x]",
    "ld d, set 4, [iy+0x%02x]",
    "ld e, set 4, [iy+0x%02x]",
    "ld h, set 4, [iy+0x%02x]",
    "ld l, set 4, [iy+0x%02x]",
    "set 4, [iy+0x%02x]",
    "ld a, set 4, [iy+0x%02x]",
    "ld b, set 5, [iy+0x%02x]",
    "ld c, set 5, [iy+0x%02x]",
    "ld d, set 5, [iy+0x%02x]",
    "ld e, set 5, [iy+0x%02x]",
    "ld h, set 5, [iy+0x%02x]",
    "ld l, set 5, [iy+0x%02x]",
    "set 5, [iy+0x%02x]",
    "ld a, set 5, [iy+0x%02x]",
    "ld b, set 6, [iy+0x%02x]",
    "ld c, set 6, [iy+0x%02x]",
    "ld d, set 6, [iy+0x%02x]",
    "ld e, set 6, [iy+0x%02x]",
    "ld h, set 6, [iy+0x%02x]",
    "ld l, set 6, [iy+0x%02x]",
    "set 6, [iy+0x%02x]",
    "ld a, set 6, [iy+0x%02x]",
    "ld b, set 7, [iy+0x%02x]",
    "ld c, set 7, [iy+0x%02x]",
    "ld d, set 7, [iy+0x%02x]",
    "ld e, set 7, [iy+0x%02x]",
    "ld h, set 7, [iy+0x%02x]",
    "ld l, set 7, [iy+0x%02x]",
    "set 7, [iy+0x%02x]",
    "ld a, set 7, [iy+0x%02x]",
    "invalid"
    ]
   })
