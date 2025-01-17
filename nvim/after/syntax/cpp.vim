" Better syntax highlighting for C++.

syn keyword cType           u8 u16 u32 u64 usize uptr i8 i16 i32 i64 isize iptr f32 f64 cstring T Status FatPtr Buffer Arena ScratchArena ArenaCheckpoint Array DynArray Option NotNull String StringView Str Vec2 IVec2 Vec3 IVec3 Vec4 IVec4 Mat3 ColMat3 Mat4 ColMat4
syn keyword cType           int8_t int16_t int32_t int64_t int_fast8_t int_fast16_t int_fast32_t int_fast64_t int_least8_t int_least16_t int_least32_t int_least64_t uint8_t uint16_t uint32_t uint64_t uint_fast8_t uint_fast16_t uint_fast32_t uint_fast64_t uint_least8_t uint_least16_t uint_least32_t uint_least64_t intmax_t intptr_t uintmax_t uintptr_t
syn keyword cppBuiltinNames nullptr typename namespace template class noexcept this using

hi def link cppBuiltinNames          Keyword
hi def link FunctionCallWithTemplate Function
hi def link FunctionCallWithTemplate2 Function
