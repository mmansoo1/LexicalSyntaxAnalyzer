import LexicalAnalyzer.{OPERATOR_PUNCTUATOR_TO_TOKEN, WORD_TO_TOKEN}

import scala.io.Source

/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Lexical Analyzer
 * Student(s) Name(s): Mohammad H. Mansoor
 */

class LexicalAnalyzer(private var source: String) extends Iterable[LexemeUnit] {

  private var input = ""
  for (line <- Source.fromFile(source).getLines)
    input += line + "\n"

  // determines the class of a given character
  private def getCharClass(c: Char): CharClass.Value = {
    if (LexicalAnalyzer.LETTERS.contains(c))
      CharClass.LETTER
    else if (LexicalAnalyzer.DIGITS.contains(c))
      CharClass.DIGIT
    else if (LexicalAnalyzer.BLANKS.contains(c))
      CharClass.BLANK
    else if (c == '+' || c == '-' || c == '*' || c == '/' ||  c == '>' || c == '<' || c == '=')
      CharClass.OPERATOR
    else if (c == '.' || c == ',' || c == ';' || c == ':')
      CharClass.PUNCTUATOR
    else if (c == '(' || c == ')')
      CharClass.DELIMITER
    else
      CharClass.OTHER
  }

  // reads the input until a non-blank character is found, returning the input updated
  private def readBlanks: Unit = {
    var foundNonBlank = false
    while (input.length > 0 && !foundNonBlank) {
      val c = input(0)
      if (getCharClass(c) == CharClass.BLANK)
        input = input.substring(1)
      else
        foundNonBlank = true
    }
  }

  def iterator: Iterator[LexemeUnit] = {
    new Iterator[LexemeUnit] {

      override def hasNext: Boolean = {
        readBlanks
        input.length > 0
      }

      override def next(): LexemeUnit = {
        if (!hasNext)
          new LexemeUnit("", Token.EOF)
        else {
          var lexeme = ""
          readBlanks
          if (input.length == 0)
            new LexemeUnit(lexeme, Token.EOF)
          else {
            var c = input(0)
            var charClass = getCharClass(c)

            // TODO: finish the code
            // Recognize a letter followed by letters (or digits) as an identifier
            if (charClass == CharClass.LETTER) {
              input = input.substring(1)
              lexeme += c
              var noMoreLettersOrDigits = false
              while (!noMoreLettersOrDigits) {
                if (input.length == 0) {
                  noMoreLettersOrDigits = true
                }
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.LETTER || charClass == CharClass.DIGIT) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else {
                    noMoreLettersOrDigits = true
                  }
                }
              }
              try {
                val reservedWord = WORD_TO_TOKEN(lexeme)
                if (reservedWord != null) {
                  return new LexemeUnit(lexeme, reservedWord)
                }
              }
              catch {
                case e: Exception =>
              }
              return new LexemeUnit(lexeme, Token.IDENTIFIER)
            }

            // Digits as literal
            if (charClass == CharClass.DIGIT) {
              input = input.substring(1)
              lexeme += c
              var noMoreDigits = false
              while (!noMoreDigits) {
                if (input.length == 0) {
                  noMoreDigits = true
                }
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.DIGIT) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else {
                    noMoreDigits = true
                  }
                }
              }
              return new LexemeUnit(lexeme, Token.INT_LITERAL)
            }
            // Operators
            else if (charClass == CharClass.OPERATOR) {
              input = input.substring(1)
              lexeme += c
              if (c == '+') {
                return new LexemeUnit(lexeme, Token.ADD_OP)
              }
              else if (c == '-') {
                return new LexemeUnit(lexeme, Token.SUB_OP)
              }
              else if (c == '*') {
                return new LexemeUnit(lexeme, Token.MUL_OP)
              }
              else if (c == '/') {
                return new LexemeUnit(lexeme, Token.DIV_OP)
              }
              else if (c == '=') {
                return new LexemeUnit(lexeme, Token.EQUALS)
              }
              else if (c == '>') {
                if (input(0) == '=') {
                  lexeme += input(0)
                  input = input.substring(1)
                  return new LexemeUnit(lexeme, Token.GREATER_EQUAL)
                }
                else {
                  return new LexemeUnit(lexeme, Token.GREATER_THAN)
                }
              }
              else if (c == '<') {
                if (input(0) == '=') {
                  lexeme += input(0)
                  input = input.substring(1)
                  return new LexemeUnit(lexeme, Token.LESS_EQUAL)
                }
                else {
                  return new LexemeUnit(lexeme, Token.LESS_THAN)
                }
              }
            }

            // Delimiters
            else if (charClass == CharClass.DELIMITER) {
              lexeme += c
              input = input.substring(1)
              if (c == '(') {
                return new LexemeUnit(lexeme, Token.OPEN_PAR)
              }
              else if (c == ')') {
                return new LexemeUnit(lexeme, Token.CLOSE_PAR)
              }
            }

            // Punctuators
            else if (charClass == CharClass.PUNCTUATOR) {
              input = input.substring(1)
              lexeme += c
              if (c == '.') {
                return new LexemeUnit(lexeme, Token.PERIOD)
              }
              else if (c == ';') {
                return new LexemeUnit(lexeme, Token.SEMICOLON)
              }
              else if (c == ':') {
                if (input(0) == '=') {
                  lexeme += input(0)
                  input = input.substring(1)
                  return new LexemeUnit(lexeme, Token.ASSGM_STMT)
                }
                else {
                  return new LexemeUnit(lexeme, Token.COLON)
                }
              }
            }
            // throw an exception if an unrecognizable symbol is found
            throw new Exception("Lexical Analyzer Error: unrecognizable symbol found!")
          }
        }
      } // end next
    } // end 'new' iterator
  } // end iterator method
} // end LexicalAnalyzer class

object LexicalAnalyzer {
  val LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS  = "0123456789"
  val BLANKS  = " \n\t"
  val WORD_TO_TOKEN = Map(
    "program"   -> Token.PROGRAM,
    "var"       -> Token.VAR,
    "begin"     -> Token.BEGIN,
    "write"     -> Token.WRITE,
    "read"      -> Token.READ,
    "Integer"   -> Token.INT_TYPE,
    "Boolean"   -> Token.BOOL_TYPE,
    "end"       -> Token.END,
    "if"        -> Token.IF,
    "else"      -> Token.ELSE,
    "while"     -> Token.WHILE,
    "true"      -> Token.BOOL_LITERAL,
    "false"     -> Token.BOOL_LITERAL,
    "do"        -> Token.DO,
    "then"      -> Token.THEN

  )
  val OPERATOR_PUNCTUATOR_TO_TOKEN = Map(
    ";"         -> Token.SEMICOLON,
    "."         -> Token.PERIOD,
    ":="        -> Token.ASSGM_STMT,
    ":"         -> Token.COLON,
    "+"         -> Token.ADD_OP,
    "-"         -> Token.SUB_OP,
    "/"         -> Token.DIV_OP,
    "*"         -> Token.MUL_OP,
    ">"         -> Token.GREATER_THAN,
    "="         -> Token.EQUALS
  )
  def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val lex = new LexicalAnalyzer(args(0))
    val it = lex.iterator
    while (it.hasNext) {
      val lexemeUnit = it.next()
      println(lexemeUnit)
    }
  } // end main method
} // end LexicalAnalyzer object