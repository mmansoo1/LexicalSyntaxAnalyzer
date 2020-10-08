/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Token
 * Student(s) Name(s): Mohammad H. Mansoor
 */

// TODO: update this enumeration with the token possible values
object Token extends Enumeration {
  val EOF        = Value
  val ADD_OP     = Value
  val SUB_OP     = Value
  val MUL_OP     = Value
  val DIV_OP     = Value
  val IDENTIFIER = Value
  val LITERAL    = Value
  val OPEN_PAR   = Value
  val CLOSE_PAR  = Value
  val IF         = Value
  val ELSE       = Value
  val WHILE      = Value
  val ASSIGNMENT = Value
  val READ       = Value
  val INT_LITERAL = Value
  val BOOL_LITERAL = Value
  val INT_TYPE   = Value
  val BOOL_TYPE  = Value
  val SEMICOLON  = Value
  val PROGRAM    = Value
  val VAR        = Value
  val BEGIN      = Value
  val WRITE      = Value
  val END        = Value
  val PERIOD     = Value
  val ASSGM_STMT = Value
  val COLON      = Value
  val GREATER_THAN = Value
  val LESS_THAN  = Value
  val GREATER_EQUAL = Value
  val LESS_EQUAL = Value
  val DO         = Value
  val THEN       = Value
  val EQUALS     = Value

}