package com.bc.LanguageUtility

import scala.collection.mutable.ListBuffer

import StringUtility.stringIsDigit

object Token:
    enum ID:
        case None, Int_Type, Integer

class Token(val id: Token.ID = Token.ID.None, val string: String = "") {}

class TokenizerSettings {}




class Tokenizer:
    private def prove(s: String): Boolean = tokenToString(stringToToken(s)) == s
    private def prove(t: Token): Boolean = stringToToken(tokenToString(t)) == t

    private def stringToToken(s: String): Token = {
        val result = s.capitalize

        if StringUtility.stringIsDigit(s) then
            return Token(Token.ID.Integer, s)

        result match
            case "Int" => Token(Token.ID.Int_Type, "Int")
            case _ => Token(Token.ID.None, "")
    }
    private def tokenToString(t: Token): String = {
        t.id match
            case Token.ID.Int_Type => "Int"
            case _ => ""
    }

    def tokenize(wordBuffer: ListBuffer[ParsedString]): Unit = {

        for word <- wordBuffer do
            if !prove(word.string) then
                println(s"Unhandled: ${word.string}")
            else
                println(s"${word.string} Is Handled!")


    }
