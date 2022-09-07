package com.bc.LanguageUtility

import scala.collection.mutable.ListBuffer

import StringUtility.stringIsDigit

object Token:
    enum ID:
        case None, NamedString,
            Int_Type, Integer,
            Equals, SemiColon
    enum Context:
        case VariableDef, FunctionDef

class Token(val id: Token.ID = Token.ID.None, val string: String = "") {}

class TokenExpression(val expr: ListBuffer[Token]):
    def getBuffer: ListBuffer[Token] = expr
    def printexpr(): Unit = {
        for i <- expr do
            print(s"'${i.string}' as ${i.id}")
    }


class TokenizerSettings {}




class Tokenizer:
    private val tokenBuffer = ListBuffer[Token]()
    private def prove(s: String): Boolean = tokenToString(stringToToken(s)) == s
    private def prove(t: Token): Boolean = stringToToken(tokenToString(t)) == t

    private def stringToToken(s: String): Token = {
        val result = s.capitalize

        if StringUtility.stringIsDigit(s) then return Token(Token.ID.Integer, s)

        result match
            case "Int" => Token(Token.ID.Int_Type, s)
            case "=" => Token(Token.ID.Equals, s)
            case ";" => Token(Token.ID.SemiColon, s)
            case _ => Token(Token.ID.None, s)
    }
    private def tokenToString(t: Token): String = {
        t.id match
            case Token.ID.Int_Type => "Int"
            case Token.ID.Equals => "="
            case Token.ID.SemiColon => ";"
            case _ => t.string
    }

    def tokenize(wordBuffer: ListBuffer[ParsedString]): Unit = {
        var lastToken = Token.ID.None
        for word <- wordBuffer do


            var isCustomName = false;

            lastToken match
                case Token.ID.Int_Type => {
                    isCustomName = true
                    tokenBuffer += Token(Token.ID.NamedString, word.string)
                    lastToken = Token.ID.NamedString;
                }
                case _ => isCustomName = false

            if !isCustomName then
            {
                if !prove(word.string) then
                    println(s"Unhandled: ${word.string}")
                    lastToken = Token.ID.None
                else
                    println(s"${word.string} Is Handled!")
                    tokenBuffer += stringToToken(word.string)
                    lastToken = stringToToken(word.string).id
            }

        println(s"Token Buffer: Size: ${tokenBuffer.size}")
        val exprBuffer = ListBuffer[Token]()
        val exprs = ListBuffer[TokenExpression]()

        tokenBuffer.foreach(x => if x.id == Token.ID.SemiColon then
                                    exprBuffer += x;
                                    exprs += TokenExpression(exprBuffer);
                                    exprBuffer.clear()
                                else
                                    exprBuffer += x)

        exprs.foreach(x => x.printexpr())

    }
