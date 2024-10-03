/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'Qunit',

  rules: {
    // TODO: what top-level constructs are allowed?
    source_file: $ => repeat(
      choice(
        $.string,
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
        $.nondet_choice,
      )
    ),

    /////////// identifiers and strings /////////// 

    identifier: $ => /[a-zA-Z_]([a-zA-Z0-9_])*/,
    // identifier_in_caps: $ => /[A-Z_]([A-Z0-9_])*/,

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
      $.identifier, // basic type
      $.operator_type,
      $.function_type,
      $.polymorphic_type,
    ),

    // TODO: what operator precedence?
    function_type: $ => prec.right(1, seq($.type, '->', $.type)),

    // TODO: what operator precedence?
    operator_type: $ => prec.right(1, seq($.type, '=>', $.type)),

    polymorphic_type: $ => seq($.identifier, '[', $.type, ']'),

    /////////// Module-level constructs ///////////
    
    module_definition: $ => seq(
      'module', $.identifier, '{',
        repeat(choice(
          $.constant_declaration,
          $.assumption,
          $.variable_definition,
          $.operator_definition,
          $.type_alias,
          $.nondet_choice,
        )),
      '}',
    ),

    constant_declaration: $ => seq(
      'const', $.identifier, ':', $.type
    ),

    assumption: $ => seq(
      'assume', $.identifier, '=', $.expr
    ),

    variable_definition: $ => seq(
      'var', $.identifier, ':', $.type
    ),

    nondet_choice: $ => seq(
      'nondet', $.identifier, '=', $.expr
    ),

    typed_argument_list: $ => withParens(
      sepByComma1(
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
        'temporal'
      ),

      // operator name:
      $.identifier,

      // argument list:
      optional($.typed_argument_list),

      // operator type:
      optional(seq(':', $.type)),

      '=', 
      $.expr, 
      optional(';')
    ),

    // TODO: https://quint-lang.org/docs/lang#module-instances

    // TODO: type alias identifier must be all CAPS
    type_alias: $ => seq('type', $.identifier, '=', $.type),

    /////////// Namespaces and Imports  ///////////

    // TODO https://quint-lang.org/docs/lang#namespaces-and-imports

    /////////// Expressions ///////////

    // TODO:
    expr: $ => choice(
      withParens($.expr),
      withBraces($.expr),
      $.bool_literal,
      $.int_literal,
      $.well_known_set,
      $.identifier,
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
      '1 to 10', // TODO
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
        withParens(sepByComma($.expr)),
      ),
      '=>',     
      $.expr
    )),

    operator_application: $ => seq(
      field('operator', $.identifier),
      field('arguments', withParens(sepByComma($.expr))),
    ),

    list_access: $ => prec.left('list_access', seq($.expr, '[', $.expr, ']')),

    unary_expr: $ => choice(
      prec('integer_neg', seq('-', $.expr)),     
    ),

    // TODO: what about `1 to 10`-like infix operator?
    // @see https://quint-lang.org/docs/lang#two-forms-of-operator-application
    binary_expr: $ => choice(
      prec.left ('ufcs_app'      , seq($.expr, '.'      , $.operator_application)),
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
      prec.left ('delayed_assign', seq($.expr, '\' ='   , $.expr)),
      prec.left ('infix_and'     , seq($.expr, 'and'    , $.expr)),
      prec.left ('infix_or'      , seq($.expr, 'or'     , $.expr)),
      prec.left ('infix_iff'     , seq($.expr, 'iff'    , $.expr)),
      prec.left ('infix_implies' , seq($.expr, 'implies', $.expr)),
      prec.left ('pair'          , seq($.expr, '->'     , $.expr)),
    ),

    braced_and: $ => prec('braced_and', seq('and', withBraces(sepEndByComma($.expr)))),
    braced_or: $ => prec('braced_or',   seq('or', withBraces(sepEndByComma($.expr)))),
    braced_all: $ => prec('braced_all', seq('all', withBraces(sepEndByComma($.expr)))),
    braced_any: $ => prec('braced_any', seq('any', withBraces(sepEndByComma($.expr)))),

    if_else_condition: $ => prec('if_else', seq('if', withParens($.expr), 'then', $.expr, 'else', $.expr)),

  },

  extras: $ => [
    $.comment,
    /\s/, // whitespace
  ],

  // conflicts: $ => [
  // ],

  precedences: $ => [
    [
      'ufcs_app',
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
      'if_else', // TODO: docs don't specify precedence
    ]
  ],

});

/**
 * Creates a rule to match one or more of the rules separated by a comma
 *
 * @param {Rule} rule
 *
 * @return {SeqRule}
 *
 */
function sepByComma1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

/**
 * Creates a rule to match zero or more of the rules separated by a comma
 *
 * @param {Rule} rule
 *
 * @return {Rule}
 *
 */
function sepByComma(rule) {
  return optional(sepByComma1(rule))
}

/**
 * Creates a rule to match one or more of the rules separated by a comma
 * and optionally ended by a comma.
 *
 * @param {Rule} rule
 *
 * @return {SeqRule}
 *
 */
function sepEndByComma1(rule) {
  return seq(sepByComma1(rule), optional(','))
}

/**
 * Creates a rule to match one or more of the rules separated by a comma
 * and optionally ended by a comma.
 *
 * @param {Rule} rule
 *
 * @return {Rule}
 *
 */
function sepEndByComma(rule) {
  return optional(sepEndByComma1(rule))
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
