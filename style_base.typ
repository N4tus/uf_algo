#import "algo_base.typ": _format_fns, eval_line, TElem
#import "types-for-typst/types_for_typst.typ": *

#let _ex_style(name, elem, styler, pred) = {
  let ex_style = (elem, style) => {
    if name != none and name in style {
      styler(elem, style)
    } else {
      panic("missing '" + name + "' in style definition")
    }
  }
  let precedence = style => {
    if name != none and name in style {
      pred(style)
    } else {
      panic("missing '" + name + "' in style definition")
    }
  }
  let style = (fn: ex_style, precedence: precedence)
  if t_check(TElem, elem) {
    elem.styles.push(style)
    elem
  } else if type(elem) == "array" {
    elem.flatten().map(e => if t_check(TElem, e) { 
      e.styles.push(style)
      e
    } else {
      (value: e, styles: (style,))
    })
  } else {
    (value: elem, styles: (style,))
  }
}

#let _choose_formatter(style, names, def_fmt, format_name) = {
  let s = style
  let style_stack = ()
  for name in names {
    if name in s {
      s = s.at(name)
      style_stack.push(s)
    } else {
      break
    }
  }
  while style_stack.len() > 0 {
    let style = style_stack.pop()
    if format_name in style {
      return style.at(format_name)
    }
  }
  def_fmt
}

#let elem(value, ..names, format: none, format_name: "format") = _ex_style(
  names.pos().at(0),
  value,
  (elem, style) => {
    let default_formatter = if format != none { format } else { i=>repr(i) }
    let formatter = _choose_formatter(style, names.pos(), default_formatter, format_name)
    formatter(elem)
  },
  style => _choose_formatter(style, names.pos(), 0, "precedence")
)

#let sp = " "
#let join(elems, sep) = if elems.len() == 0 {()} else if elems.len() == 1 { elems.at(0) } else { range(elems.len()*2-1).map(i => if calc.even(i) {elems.at(int(i/2))} else {sep}) }
#let ge = (value: (group: none), styles: ())
#let gs(fn, use_start: false, use_end: false) = (value: (group: fn, use_start: use_start, use_end: use_end), styles: ())

#let pipe(..fns) = s => {
  let _s = s

  for fn in fns.pos() {
    let r = fn(_s)
    // first element, if successful, second the value
    if type(r) == "array" {
      // if successful, stop otherwise continue
      if r.at(0) {
        return r.at(1)
      }
    } else {
      _s = r
    }
  }
  _s
}
#let _if(cond, ..fns) = s => {
  if cond(s) {
    let p = pipe(..fns)
    p(s)
  } else {
    s
  }
}

#let _if_else(cond, ..fns) = s => {
  if cond(s) {
    let p = pipe(..fns)
    (true, p(s))
  } else {
    (false, none)
  }
}

#let if_string(..fns) = _if(s => type(s) == "string", ..fns)
#let if_number(..fns) = _if(s => type(s) == "integer" or type(s) == "float", ..fns)
#let if_string_else(..fns) = _if_else(s => type(s) == "string", ..fns)
#let if_number_else(..fns) = _if_else(s => type(s) == "integer" or type(s) == "float", ..fns)
