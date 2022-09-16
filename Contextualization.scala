package com.bc.LanguageUtility


import com.bc.LanguageUtility.*

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._


class ContextBuffer(val tokens: ListBuffer[Token]):
    def print_(): Unit = {
        tokens.foreach(x => print(s"'${x.string}' ") )
        println()
    }
    def printID_(): Unit = {
        tokens.foreach(x => print(s"'${x.string}' as ${x.id} ") )
        println()
    }

class ContextualizationSettings(val newLinesEndStatements: Boolean = false):
    ;

class Contextualization(val contextualizationSettings: ContextualizationSettings = ContextualizationSettings()):
    enum Context:
        case None, VariableDef, FunctionDef, VariableCall, FunctionCall

    private val tokenBuffer = ListBuffer[Token]()
    private var context = Context.None


    def contextualize(tokens: List[Token], startIndex: Int = 0): (ListBuffer[Token], Int) = {
        val list: List[Token] = if startIndex > 0 then
                                    for i <- tokens.zipWithIndex if i._2 > startIndex yield i._1
                                else
                                    tokens
        var index = startIndex
        breakable {
            for i <- list.zipWithIndex do
                tokenBuffer += i._1
                index += 1
                if i._1.id == Token.ID.SemiColon then
                    if i._2 < list.length - 1 && (list(i._2+1).id == Token.ID.NewLine && contextualizationSettings.newLinesEndStatements) then
                        index += 1
                    break()
                else if i._1.id == Token.ID.Int_Type || i._1.id == Token.ID.String_Type then
                    context = Context.VariableDef
        }
        index += 1
        (tokenBuffer, index)
    }

    // TODO: Implement
    def contextualizeAll(tokens: List[Token]): ListBuffer[ContextBuffer] = {
        val tokenList = ListBuffer[ContextBuffer]()
        var index = 0
        while index < tokens.length do
            val stmt = contextualize(tokens, index)
            index += stmt._2
            tokenList += ContextBuffer(stmt._1)
        tokenList
    }


