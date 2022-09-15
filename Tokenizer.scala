package com.bc.LanguageUtility

import scala.collection.mutable.ListBuffer
import StringUtility.{stringIsDigit, stringIsString}

import scala.util.control.Breaks

object Token:
    enum ID:
        case None,
            NamedString,
            Int_Type,
            Integer,
            String_Type,
            String,
            Equals,
            SemiColon,
            NewLine

class Token(val id: Token.ID = Token.ID.None, val string: String = ""):
    def print(): Unit = println(s"'$string'")
    def printID(): Unit = println(s"'$string' as $id")

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
            case "String" => Token(Token.ID.String_Type, s)
            case "=" => Token(Token.ID.Equals, s)
            case ";" => Token(Token.ID.SemiColon, s)
            case "\n" => Token(Token.ID.NewLine, s)
            case "\\n" => Token(Token.ID.NewLine, s)
            case _ => Token(Token.ID.None, s)
    }
    private def tokenToString(t: Token): String = {
        t.id match
            case Token.ID.Int_Type => "Int"
            case Token.ID.String_Type => "String"
            case Token.ID.Equals => "="
            case Token.ID.SemiColon => ";"
            case Token.ID.NewLine => "\\n"
            case _ => t.string
    }

    def tokenize(wordBuffer: ListBuffer[ParsedString]): ListBuffer[Token] = {
        var lastToken = Token.ID.None
        var variableType = Token.ID.None
        val userString = Breaks();
        for word <- wordBuffer do
            userString.breakable {

                if word.string == ";" then
                    variableType = Token.ID.None

                if variableType == Token.ID.String_Type || StringUtility.stringIsString(word.string) then
                    tokenBuffer += Token(Token.ID.String, word.string)
                    userString.break()


                lastToken match
                    case Token.ID.Int_Type =>
                        tokenBuffer += Token(Token.ID.NamedString, word.string)
                        lastToken = Token.ID.NamedString
                        variableType = Token.ID.Int_Type
                        userString.break()
                    case Token.ID.String_Type =>
                        tokenBuffer += Token(Token.ID.NamedString, word.string)
                        lastToken = Token.ID.NamedString
                        variableType = Token.ID.String_Type
                        userString.break()
                    case _ => ;

                if prove(word.string) then
                    tokenBuffer += stringToToken(word.string)
                    lastToken = stringToToken(word.string).id
                else
                    println(s"Unhandled: ${word.string}")
                    lastToken = Token.ID.None
            }
        tokenBuffer
    }
