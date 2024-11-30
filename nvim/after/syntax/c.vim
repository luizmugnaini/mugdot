" Better syntax highlighting for C.

syn keyword cType    u8 u16 u32 u64 usize uptr i8 i16 i32 i64 isize iptr f32 f64 strptr cstring T int8_t int16_t int32_t int64_t int_fast8_t int_fast16_t int_fast32_t int_fast64_t int_least8_t int_least16_t int_least32_t int_least64_t uint8_t uint16_t uint32_t uint64_t uint_fast8_t uint_fast16_t uint_fast32_t uint_fast64_t uint_least8_t uint_least16_t uint_least32_t uint_least64_t intmax_t intptr_t uintmax_t uintptr_t ptrdiff_t size_t ssize_t
syn keyword cBoolean true false TRUE FALSE

syn match cCustomParen transparent "(" contains=cParen contains=cCppParen
syn match cCustomFunc  "\w\+\s*(\@=" contains=cCustomParen

syn keyword cAnsiFunction	main typeof assert offsetof alignof alignas INT8_C INT16_C INT32_C INT64_C UINT8_C UINT16_C UINT32_C UINT64_C UINTMAX_C INTMAX_C
syn keyword	cAnsiName	    errno environ STDC INT8_MAX INT16_MAX INT32_MAX INT64_MAX INT8_MIN INT16_MIN INT32_MIN INT64_MIN INT_FAST8_MAX INT_FAST16_MAX INT_FAST32_MAX INT_FAST64_MAX INT_FAST8_MIN INT_FAST16_MIN INT_FAST32_MIN INT_FAST64_MIN INT_LEAST8_MAX INT_LEAST16_MAX INT_LEAST32_MAX INT_LEAST64_MAX INT_LEAST8_MIN INT_LEAST16_MIN INT_LEAST32_MIN INT_LEAST64_MIN INTMAX_MAX INTMAX_MIN INTPTR_MAX INTPTR_MIN PTRDIFF_MAX PTRDIFF_MIN SIG_ATOMIC_MAX SIG_ATOMIC_MIN SIZE_MAX WCHAR_MAX WCHAR_MIN WINT_MAX WINT_MIN UINTMAX_MAX UINTPTR_MAX  UINT8_MAX UINT16_MAX UINT32_MAX UINT64_MAX UINT_FAST8_MAX UINT_FAST16_MAX UINT_FAST32_MAX UINT_FAST64_MAX UINT_LEAST8_MAX UINT_LEAST16_MAX UINT_LEAST32_MAX UINT_LEAST64_MAX

hi def link cIdentifier   Identifier
hi def link cBoolean      Boolean
hi def link cFunction     Function
hi def link cCustomFunc   Function
hi def link cAnsiFunction cFunction
hi def link cAnsiName     cIdentifier
