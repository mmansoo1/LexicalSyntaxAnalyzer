/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Syntax Analyzer
 * Student(s) Name(s): Mohammad H. Mansoor
 */

class SyntaxAnalyzer(private var source: String) {

  private var it = new LexicalAnalyzer(source).iterator
  private var lexemeUnit: LexemeUnit = null

  private def getLexemeUnit() = {
    if (lexemeUnit == null)
      lexemeUnit = it.next()
  }

  def parse(): Tree = {
    parseProgram()
  }

  // TODO: finish the syntax analyzer
  // program = `program` identifier body `.`
  private def parseProgram() = {
    // create a tree with label "program"
    val tree = new Tree("program")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      // If token is 'program', add token as new branch and reset lexemeUnit
      // Then add result of 'parseIdentifier and 'parseBody' as new branches
      if (lexemeUnit.getToken() == Token.PROGRAM) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        if (lexemeUnit.getToken() == Token.IDENTIFIER) {
          tree.add(parseIdentifier())
        }
        else {
          throw new Exception("Syntax Analyzer Error: identifier expected!")
        }
        tree.add(parseBody())
        if (lexemeUnit.getToken() == Token.PERIOD) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          getLexemeUnit()
          if (lexemeUnit.getToken() != Token.EOF) {
            throw new Exception("Syntax Analyzer Error: EOF was expected!")
          }
        }
        else {
          throw new Exception("Syntax Analyzer Error: period was expected!")
        }
      }
    }
    else {
      throw new Exception("Syntax Analyzer Error: \"program\" was expected!")

    }
    // return the tree
    tree
  }

  // body = [ var_sct ] block
  // this really just means: body = block | var_sect block
  private def parseBody(): Tree = {
    val tree = new Tree("body")

    lexemeUnit = null
    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.VAR) {
        tree.add(parseVarSect())
        tree.add(parseBlock())
      }
      else {
        tree.add(parseBlock())
      }
    }
    else {
      throw new Exception("Syntax Analyzer Error: \"var section\" or \"block\" was expected!")
    }

    tree
  }

  // var_sct -> ´var´ var_dcl { ´;´ var_dcl }
  private def parseVarSect(): Tree = {
    val tree = new Tree("var_sect")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.VAR) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        tree.add(parseVarDcl())
        while (lexemeUnit.getToken() == Token.SEMICOLON) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          tree.add(parseVarDcl())
        }
      }
    }
    else {
      throw new Exception("Syntax Analyzer Error: \"var\" was expected!")
    }

    tree
  }

  // var_dcl = identifier { identifier } `:` type
  private def parseVarDcl(): Tree = {
    val tree = new Tree("var_dcl")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      while (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseIdentifier())
        lexemeUnit = null
        getLexemeUnit()
      }
      if (lexemeUnit.getToken() == Token.COLON) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        tree.add(parseType())
      }
      else {
        throw new Exception("Syntax Analyer Error: colon was expected!")
      }
    }
    else {
      throw new Exception("Syntax Analyzer Error: \"identifier\" was expected!")
    }

    tree
  }

  // type = ´Integer´ | ´Boolean´
  private def parseType(): Tree = {
    val tree = new Tree("type")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if(lexemeUnit.getToken() == Token.INT_TYPE || lexemeUnit.getToken() == Token.BOOL_TYPE) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }
      else {
        throw new Exception("Syntax Analyzer Error: type was expected!")
      }
    }
    tree
  }

  // block = ´begin´ stmt { ´;´ stmt } end
  private def parseBlock(): Tree = {
    val tree = new Tree("block")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.BEGIN) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        tree.add(parseStmt())
        getLexemeUnit()
        while (lexemeUnit.getToken() == Token.SEMICOLON) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          tree.add(parseStmt())
        }
        if (lexemeUnit.getToken() != Token.END) {
          lexemeUnit = null
          getLexemeUnit()
        }
        if (lexemeUnit.getToken() == Token.END) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          getLexemeUnit()
        }
        else {
          throw new Exception("Syntax Analyzer Error: end was expected!")
        }
      }
      else {
        throw new Exception("Syntax Analyzer Error: begin was expected!")
      }
    }

    else {
      throw new Exception("Syntax Analyzer Error: Begin statement expected!")
    }

    tree
  }

  // stmt = assgm_stmt | read_stmt | write_stmt | if_stmt | while_stmt | block
  private def parseStmt(): Tree = {
    val tree = new Tree("stmt")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseAssgmStmt())
      }
      else if (lexemeUnit.getToken() == Token.READ) {
        tree.add(parseReadStmt())
      }
      else if (lexemeUnit.getToken() == Token.WRITE) {
        tree.add(parseWriteStmt())
      }
      else if (lexemeUnit.getToken() == Token.IF) {
        tree.add(parseIfStmt())
      }
      else if (lexemeUnit.getToken() == Token.WHILE) {
        tree.add(parseWhileStmt())
      }
      else {
        tree.add(parseBlock())
      }
    }

    tree
  }

  // assgm_stmt = identifier ´:=´ expr
  private def parseAssgmStmt(): Tree = {
    val tree = new Tree("assgm_stmt")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseIdentifier())
        lexemeUnit = null
        getLexemeUnit()
        if (lexemeUnit.getToken() == Token.ASSGM_STMT) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          getLexemeUnit()
          tree.add(parseExpr())
        }
        else {
          throw new Exception("Syntax Analyzer Error: assignment statement was expected!")
        }
      }
    }

    tree
  }

  // read_stmt = ´read´ identifier
  private def parseReadStmt(): Tree = {
    val tree = new Tree("read_stmt")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.READ) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseIdentifier())
        lexemeUnit = null
        getLexemeUnit()
      }
      else {
        throw new Exception("Syntax Analyzer Error: \"read statement\" expected!")
      }
    }
    tree
  }

  // write_stmt = ´write´ ( identifier | literal )
  private def parseWriteStmt(): Tree = {
    val tree = new Tree("write_stmt")

    getLexemeUnit()

    if (lexemeUnit.getToken != Token.EOF) {
      if (lexemeUnit.getToken == Token.WRITE) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        if (lexemeUnit.getToken() == Token.IDENTIFIER) {
          tree.add(parseIdentifier())
          //lexemeUnit = null
        }
        else if (lexemeUnit.getToken() == Token.LITERAL) {
          tree.add(parseLiteral())
          lexemeUnit = null
        }
        else {
          throw new Exception("Syntax Analyzer Error: \"identifier\" or \"literal\" expected!")
        }
      }
    }

    tree
  }

  // if_stmt = ´if´ bool_expr ´then´ stmt [ ´else´ stmt ]
  private def parseIfStmt(): Tree = {
    val tree = new Tree("if_stmt")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.IF) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        tree.add(parseBoolExpr())
        if (lexemeUnit.getToken() == Token.THEN) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          tree.add(parseStmt())
          getLexemeUnit()
          if (lexemeUnit.getToken() == Token.ELSE) {
            tree.add(new Tree(lexemeUnit.getLexeme()))
            lexemeUnit = null
            tree.add(parseStmt())
          }
        }
        else {
          throw new Exception("Syntax Analyzer Error: then expected!")
        }
      }
    }
    tree
  }

  // while_stmt = ´while´ bool_expr ´do´ stmt
  private def parseWhileStmt(): Tree = {
    val tree = new Tree("while_stmt")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.WHILE) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseBoolExpr())
        if (lexemeUnit.getToken() == Token.DO) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          tree.add(parseStmt())
        }
        else {
          throw new Exception("Syntax Analyzer Error: do expected!")
        }
      }
    }

    tree
  }

  // expr = arithm_expr | bool_expr
  private def parseExpr(): Tree = {
    val tree = new Tree("expr")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
        if (lexemeUnit.getToken() == Token.IDENTIFIER || lexemeUnit.getToken() == Token.INT_LITERAL) {
        tree.add(parseArithmExpr())
      }
      else if (lexemeUnit.getToken() == Token.BOOL_LITERAL || lexemeUnit.getToken() == Token.GREATER_THAN || lexemeUnit.getToken() == Token.GREATER_EQUAL
        || lexemeUnit.getToken() == Token.EQUALS || lexemeUnit.getToken() == Token.LESS_EQUAL || lexemeUnit.getToken() == Token.LESS_THAN) {
        tree.add(parseBoolExpr())
        lexemeUnit = null
        getLexemeUnit()
      }
      else {
        throw new Exception("Syntax Analyzer Error: identifier or int literal expected!")
      }
    }
    tree
  }

  // arithm_expr = term arithm_expr'
  private def parseArithmExpr(): Tree = {
    val tree = new Tree("arithm_expr")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parseTerm())
      tree.add(parseArithmExprPrime())
    }

    else {
      throw new Exception("Syntax Analyzer Error: term was expected!")
    }
    tree
  }

  // arithm_expr' = ('+' | '-') term arithm_expr' | epsilon
  private def parseArithmExprPrime(): Tree = {
    val tree = new Tree("arithm_expr'")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.ADD_OP || lexemeUnit.getToken() == Token.SUB_OP) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        tree.add(parseTerm())
        tree.add(parseArithmExprPrime())
      }
    }
    tree
  }

  // term = factor term'
  private def parseTerm(): Tree = {
    val tree = new Tree("term")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parseFactor())
      tree.add(parseTermPrime())
    }

    tree
  }

  // term' = '*' factor term' | epsilon
  private def parseTermPrime(): Tree = {
    val tree = new Tree("term")

    getLexemeUnit()
    if (lexemeUnit.getToken != Token.EOF) {
      if (lexemeUnit.getToken() == Token.MUL_OP) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        tree.add(parseFactor())
        tree.add(parseTermPrime())
      }
    }

    tree
  }

  // factor = identifier | int_literal
  private def parseFactor(): Tree = {
    val tree = new Tree("factor")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseIdentifier())
        lexemeUnit = null
      }
      else if (lexemeUnit.getToken() == Token.INT_LITERAL) {
        tree.add(parseIntLiteral())
        lexemeUnit = null
      }
    }

    tree
  }

  // literal = int_literal | bool_literal
  private def parseLiteral(): Tree = {
    val tree = new Tree("literal")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.INT_LITERAL) {
        tree.add(parseIntLiteral())
        lexemeUnit = null
      }
      else if (lexemeUnit.getToken() == Token.BOOL_LITERAL) {
        tree.add(parseBoolLiteral())
        lexemeUnit = null
      }
    }

    tree
  }

  // int_literal = digit { digit }
  private def parseIntLiteral() = new Tree("literal: '" + lexemeUnit.getLexeme() + "'")

  // bool_litreal = ´true´ | ´false´
  private def parseBoolLiteral(): Tree = {
    val tree = new Tree("bool_literal")

    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.BOOL_LITERAL) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
      }
    }

    tree
  }

  // bool_expr = bool_literal | arithm_expr ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ ) arithm_expr
  private def parseBoolExpr(): Tree = {
    val tree = new Tree("bool_expr")

    getLexemeUnit()

    if (lexemeUnit.getToken != Token.EOF) {
      if (lexemeUnit.getToken() == Token.BOOL_LITERAL) {
        tree.add(parseBoolLiteral())
      }
      else {
        tree.add(parseArithmExpr())
        if (lexemeUnit.getToken == Token.BOOL_LITERAL || lexemeUnit.getToken == Token.GREATER_THAN || lexemeUnit.getToken() == Token.GREATER_EQUAL
          || lexemeUnit.getToken() == Token.EQUALS || lexemeUnit.getToken() == Token.LESS_EQUAL || lexemeUnit.getToken() == Token.LESS_THAN) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          tree.add(parseArithmExpr())
        }
        else {
          throw new Exception("Syntax Analyzer Error: relational operator expected!")
        }
      }
    }

    tree
  }

  // identifier = letter { ( letter | digit ) }
  private def parseIdentifier() = new Tree("identifier: '" + lexemeUnit.getLexeme() + "'")
}

object SyntaxAnalyzer {
  def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse()
    print(parseTree)
  }
}