; Highlights configured for Helix editor.
; See: https://docs.helix-editor.com/themes.html#scopes

[
  "module"
  ; "from"
  ; "as"
  "type"
  "assume"
  "const"
  "var"
  "val"
  "nondet"
  "def"
  "pure"
  "action"
  "temporal"
  "run"
] @keyword

(match_expr "match" @keyword.control.conditional)

(if_else_condition 
  "if" @keyword.control.conditional
  "else" @keyword.control.conditional)

; [
;   "or"
;   "and"
;   "implies"
;   "iff"
;   "all"
;   "any"
;   ; "not"
; ] @keyword.operator

; [
;   "import"
;   "export"
; ] @keyword.control.import

[
  "true"
  "false"
  "Int"
  "Nat"
  "Bool"
] @constant.builtin

[
  ";"
  "."
  ","
] @punctuation.delimiter

[
  "-"
  "+"
  "*"
  "/"
  "%"
  "<"
  "<="
  "="
  "=="
  "!="
  "=>"
  ">"
  ">="
  "^"
  "->"
] @operator

(infix_and "and" @operator)
(infix_or "or" @operator)
(infix_iff "iff" @operator)
(infix_implies "implies" @operator)

(braced_and "and" @keyword)
(braced_or "or" @keyword)
(braced_all "all" @keyword)
(braced_any "any" @keyword)

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

(polymorphic_type 
  (type) @type.parameter)

(variant_constructor) @type.enum.variant

(type) @type
(int_literal) @constant.numeric.integer
(comment) @comment
(string) @string

(operator_application
  operator: (identifier) @function)

; operator definition is a function if it has at least one argument ...
(operator_definition
  name: (identifier) @function
  arguments: (typed_argument_list))
; ... or if the right-hand-side is a lambda expression: 
; (operator_definition
;   name: (identifier) @function
;   rhs: (lambda_expr))
