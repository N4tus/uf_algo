#import "types-for-typst/types_for_typst.typ": *

#let TMakeup = TDict(style: TAny, format: TAny)
#let AElem(t_value) = TDict(value: t_value, styles: TArray(TDict(fn: TFunction, precedence: TFunction))) 
#let TElem = AElem(TAny)

#let AGroup(t_group) = TDict(group: t_group, start_style: optional(TFunction), end_style: optional(TFunction), use_start: optional(TBoolean), use_end: optional(TBoolean))
#let TGroup = AGroup(t_or(TFunction, TNone))
#let TGroupElem = AElem(TGroup)
#let TGroupStart = AGroup(TFunction)
#let TGroupEnd = AGroup(TNone)

#let TBody = TDict(body: TAny, indent: TFunction)
