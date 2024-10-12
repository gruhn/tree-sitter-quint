/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'quint',

  rules: {
    // TODO: what top-level constructs are allowed?
    source_file: $ => repeat(
      choice(
        $.module_definition,

        // Technically, these are not valid top-level statements
        // but it probably makes sence that the tree-sitter grammar is more
        // lenient, so these constructs are properly highlighted even 
        // outside a module:
        $.constant_declaration,
        $.assumption,
        $.variable_definition,
        $.operator_definition,
        $.type_alias,
        $.string,
        $.import,
        $.export
      )
    ),

    /////////// identifiers and strings /////////// 

    identifier: $ => /[a-zA-Z_]([a-zA-Z0-9_])*/,
    // identifier_in_caps: $ => /[A-Z_]([A-Z0-9_])*/,

    // TODO: are there places where only non-qualified identifieres are allowed?
    qualified_identifier: $ => seq(
      repeat(seq($.identifier, '::')),
      $.identifier
    ),

    unescaped_double_string_fragment: _ => token.immediate(prec(1, /[^"\\\r\n]+/)),

    // QUESTION: correct escape syntax?
    escape_sequence: _ => token.immediate(seq(
      '\\',
      choice(
        /[^xu0-7]/,
        /[0-7]{1,3}/,
        /x[0-9a-fA-F]{2}/,
        /u[0-9a-fA-F]{4}/,
        /u\{[0-9a-fA-F]+\}/,
        /[\r?][\n\u2028\u2029]/,
      ),
    )),

    string: $ => seq(
      '"',
      repeat(choice(
        alias($.unescaped_double_string_fragment, $.string_fragment),
        $.escape_sequence,
      )),
      '"',
    ),

    /////////// comments /////////// 
   
    comment: _ => token(choice(
      // single line comment:
      seq('//', /[^\r\n\u2028\u2029]*/),
      // multi-line comment:
      seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/',
      ),
    )),

    /////////// Types ///////////

    // TODO: all types covered?
    type: $ => choice(
      prec('basic_type', $.qualified_identifier), // basic type
      $.operator_type,
      $.function_type,
      $.polymorphic_type,
      $.record_type,
      $.tuple_type,
    ),

    // QUESTION: function types always have exaclty one argument?
    function_type: $ => prec.right('function_type', seq($.type, '->', $.type)),

    operator_type: $ => prec.right('operator_type', seq(
      choice(
        $.type, // single argument / no parens, e.g. `int => bool`
        withParens(sepBy(',', $.type)), // zero or more arguments, e.g. `(int, int) => int`
      ),
      "=>",
      $.type, // result type
    )),

    polymorphic_type: $ => seq(
      $.qualified_identifier, 
      withBrackets(sepBy1(',', $.type))
    ),

    // TODO: can variant constructors have more than one argument?
    variant_constructor: $ => prec('variant_constr', seq(
      $.qualified_identifier, 
      optional(withParens($.type))
    )),

    // We say sum types must have at least two variant constructor.
    // One variant constructor makes sense, but can't be distingished
    // with a basic type like `int`.
    sum_type: $ => seq(
      optional('|'), // optional leading pipe
      sepBy1('|', $.variant_constructor)
    ),

    record_type: $ => withBraces(
      sepEndBy1(',', seq(
        seq($.qualified_identifier, ':', $.type),
      ))
    ),

    tuple_type: $ => prec('tuple_type', withParens(sepBy(',', $.type))),

    /////////// Module-level constructs ///////////
    
    module_definition: $ => seq(
      'module', $.qualified_identifier, '{',
        repeat(choice(
          $.constant_declaration,
          $.assumption,
          $.variable_definition,
          $.operator_definition,
          $.type_alias,
          $.import,
          $.export,
        )),
      '}',
    ),

    constant_declaration: $ => seq(
      'const', $.qualified_identifier, ':', $.type
    ),

    assumption: $ => seq(
      'assume', $.qualified_identifier, '=', $.expr
    ),

    variable_definition: $ => seq(
      'var', $.qualified_identifier, ':', $.type
    ),

    typed_argument_list: $ => withParens(
      sepBy(
        ',',
        seq(
          $.identifier,
          optional(seq(':', $.type))
        )
      ), 
    ),

    operator_definition: $ => seq(
      // operator kind:
      choice(
        seq(
          optional('pure'), 
          choice('val', 'def')
        ),
        'action', 
        'temporal',
        'nondet',
        'run'
      ),

      // operator name:
      field('name', $.qualified_identifier),

      // argument list:
      field('arguments', optional($.typed_argument_list)),

      // operator type:
      optional(seq(':', $.type)),

      '=', 
      field('rhs', $.expr), 
      choice(';', '\n')
    ),

    // TODO: https://quint-lang.org/docs/lang#module-instances

    // TODO: type alias identifier must be all CAPS
    type_alias: $ => seq(
      'type', 
      $.qualified_identifier, 
      optional(withBrackets(sepBy1(',', $.identifier))), // optional type arguments
      '=', 
      choice(
        $.type, 
        $.sum_type,
      )
    ),

    import: $ => seq(
      'import', 
      sepBy1('.', choice($.qualified_identifier, '*')),
      optional(
        seq('as', $.qualified_identifier)
      ),
      optional( // when imported from another file
        seq('from', $.string)
      )    
    ),

    export: $ => seq(
      'export', 
      sepBy1('.', choice($.qualified_identifier, '*')),
      optional(
        seq('as', $.qualified_identifier)
      ),
    ),

    /////////// Expressions ///////////

    // TODO:
    expr: $ => choice(
      prec('parens', withParens($.expr)),
      withBraces($.expr),
      $.bool_literal,
      $.int_literal,
      $.well_known_set,
      $.qualified_identifier,
      $.lambda_expr,
      $.operator_application,
      $.unary_expr,
      $.binary_expr,
      $.list_access,
      $.braced_and,
      $.braced_or,
      $.braced_any,
      $.braced_all,      
      $.string,
      $.if_else_condition,
      $.local_operator_definition,
      $.record_literal,
      $.tuple_literal,
      $.list_literal,
      $.match_expr,
    ),

    well_known_set: $ => choice('Bool', 'Int', 'Nat'),

    bool_literal: $ => choice('true', 'false'),

    int_literal: _ => {
      const hexLiteral = seq(
        choice('0x', '0X'),
        /[\da-fA-F](_?[\da-fA-F])*/,
      );

      const decimalDigits = /\d(_?\d)*/;

      const decimalIntegerLiteral = choice(
        '0',
        seq(optional('0'), /[1-9]/, optional(seq(optional('_'), decimalDigits))),
      );

      return token(choice(
        hexLiteral,
        decimalIntegerLiteral,
      ));
    },


    lambda_expr: $ => prec.right(1, seq(
      choice(
        $.identifier,
        // TODO: can lambdas args have type annotations?
        // TODO: pattern matching in lambda args.
        withParens(sepBy(',', $.expr)),
      ),
      '=>',     
      $.expr
    )),

    operator_application: $ => seq(
      field('operator', $.qualified_identifier),
      field('arguments', withParens(sepBy(',', $.expr))),
    ),

    list_access: $ => prec.left('list_access', seq($.expr, '[', $.expr, ']')),

    unary_expr: $ => choice(
      prec('integer_neg', seq('-', $.expr)),     
    ),

    // TODO: what about `1 to 10`-like infix operator?
    // @see https://quint-lang.org/docs/lang#two-forms-of-operator-application
    binary_expr: $ => choice(
      prec.left ('ufcs'          , seq($.expr, '.'      , $.expr)),
      prec.right('integer_exp'   , seq($.expr, '^'      , $.expr)),
      prec.left ('integer_mult'  , seq($.expr, '*'      , $.expr)),
      prec.left ('integer_mult'  , seq($.expr, '/'      , $.expr)),
      prec.left ('integer_mult'  , seq($.expr, '%'      , $.expr)),
      prec.left ('integer_plus'  , seq($.expr, '+'      , $.expr)),
      prec.left ('integer_plus'  , seq($.expr, '-'      , $.expr)),
      prec.left ('comparison'    , seq($.expr, '<'      , $.expr)),
      prec.left ('comparison'    , seq($.expr, '>'      , $.expr)),
      prec.left ('comparison'    , seq($.expr, '>='     , $.expr)),
      prec.left ('comparison'    , seq($.expr, '<='     , $.expr)),
      prec.left ('comparison'    , seq($.expr, '=='     , $.expr)),
      prec.left ('comparison'    , seq($.expr, '!='     , $.expr)),
      $.infix_and,
      $.infix_or,
      $.infix_iff,
      $.infix_implies,
      prec.left ('delayed_assign', seq($.expr, '\' ='   , $.expr)),
      prec.left ('pair'          , seq($.expr, '->'     , $.expr)),
    ),

    infix_and: $ => prec.left('infix_and', seq($.expr, 'and', $.expr)),
    infix_or: $ => prec.left('infix_or', seq($.expr, 'or', $.expr)),
    infix_iff: $ => prec.left('infix_iff', seq($.expr, 'iff', $.expr)),
    infix_implies: $ => prec.left ('infix_implies' , seq($.expr, 'implies', $.expr)),

    braced_and: $ => prec('braced_and', seq('and', withBraces(sepEndBy(',', $.expr)))),
    braced_or: $ => prec('braced_or',   seq('or', withBraces(sepEndBy(',', $.expr)))),
    braced_all: $ => prec('braced_all', seq('all', withBraces(sepEndBy(',', $.expr)))),
    braced_any: $ => prec('braced_any', seq('any', withBraces(sepEndBy(',', $.expr)))),

    if_else_condition: $ => prec('if_else', seq('if', withParens($.expr), $.expr, 'else', $.expr)),

    local_operator_definition: $ => prec.right('local_def', seq($.operator_definition, $.expr)),

    record_literal: $ => withBraces(
      sepBy1(
        ',',
        choice(
          seq($.qualified_identifier, ':', $.expr),
          seq('...', $.qualified_identifier) // record spread
        )
      )
    ),

    tuple_literal: $ => prec('tuple', withParens(sepBy(',', $.expr))),

    match_expr: $ => seq('match', $.expr, withBraces(
      repeat(seq('|', $.qualified_identifier, optional(withParens($.expr)), '=>', $.expr))
    )),

    list_literal: $ => withBrackets(sepBy(',', $.expr)),

    // TODO: nested operator definitions

  },

  extras: $ => [
    $.comment,
    /\s/, // whitespace
  ],

  // conflicts: $ => [
  // ],

  precedences: $ => [
    [
      'ufcs',
      'list_access',
      'integer_neg',
      'integer_exp',
      'integer_mult',
      'integer_plus',
      'comparison',
      'delayed_assign',
      'braced_and',
      'infix_and',
      'braced_or',
      'infix_or',
      'infix_iff',
      'infix_implies',
      'pair',
      'braced_all',
      'braced_any',
      'local_def',
      'if_else', // TODO: docs don't specify precedence
    ],
    [
      'tuple',
      'parens',
    ],
    [
      'function_type',
      'operator_type',
      'variant_constr',
      'tuple_type',
      'basic_type',
    ]
  ],

});

/**
 *
 * @param {string} sep
 * @param {Rule} rule
 *
 * @return {SeqRule}
 *
 */
function sepBy1(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)))
}

/**
 *
 * @param {string} sep
 * @param {Rule} rule
 *
 * @return {Rule}
 *
 */
function sepBy(sep, rule) {
  return optional(sepBy1(sep, rule))
}

/**
 *
 * @param {string} sep
 * @param {Rule} rule
 *
 * @return {Rule}
 *
 */function sepEndBy1(sep, rule) {
  return seq(sepBy1(sep, rule), optional(','))
}

/**
 *
 * @param {string} sep
 * @param {Rule} rule
 *
 * @return {Rule}
 *
 */function sepEndBy(sep, rule) {
  return optional(sepEndBy1(sep, rule))
}

/**
 * @param {Rule} rule
 *
 * @return {Rule}
 *
 */
function withParens(rule) {
  return seq('(', rule, ')')
}

/**
 * @param {Rule} rule
 *
 * @return {Rule}
 *
 */
function withBraces(rule) {
  return seq('{', rule, '}')
}

/**
 * @param {Rule} rule
 *
 * @return {Rule}
 *
 */
function withBrackets(rule) {
  return seq('[', rule, ']')
}
