" Vim syntax file
" Language:     Base PC Assembly
" Version:      0.1
" Remark:       Covers base instruction set only
" License:      CC0 1.0 (PublicDomain)
"

" Main commands

syn keyword bpcAddrCmd ISZ AND JSR MOV ADD ADC SUB BCS BPL BMI BEQ BR nextgroup=bpcAddr,bpcAddrLabel skipwhite
syn keyword bpcFixedCmd HLT NOP CLA CLC CMA CMC ROL ROR INC DEC EI DI
syn keyword bpcIOCmd CLF TSF IN OUT

syn match bpcAddrLabel /[(]\?[_$.a-zA-Z0-9]\+[)]\?/ contained 
syn match bpcAddr /[(]\?[a-fA-F0-9]\+[)]\?\([^a_$.a-zA-Z0-9]\|$\)/ contained
syn match bpcIndirectAddrParens /[()]/ contained containedin=bpcAddr,bpcAddrLabel

" Assembly-specific constructs

syn keyword bpcAsmKwd ORG WORD DUP nextgroup=bpcAsmConst skipwhite

syn match bpcAsmConst /?/ contained
syn match bpcAsmConst /[0-9a-fA-F]\+/ contained

syn match bpcAsmLabel /[_$.a-zA-Z0-9]\+:/

syn region bpcAsmComment start=/;/ end=/$/

" Mappings

hi def link bpcAddrCmd Statement
hi def link bpcFixedCmd Statement
hi def link bpcIOCmd Statement

hi def link bpcAddr Number
hi def link bpcAddrLabel Constant
hi def link bpcIndirectAddrParens Statement

hi def link bpcAsmLabel Constant

hi def link bpcAsmKwd Identifier
hi def link bpcAsmConst Number

hi def link bpcAsmComment Comment
