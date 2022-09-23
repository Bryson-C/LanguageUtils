package com.bc.LanguageUtility

// TODO: Add Documentation For LanguageUtility Package
import com.bc.LanguageUtility._

import scala.collection.mutable.ListBuffer



@main def main(): Unit = {

    val settings = ParserSettings("D:\\Scala\\src\\main\\data.lang", true)
    val parser = Parser();
    parser.parse(settings)
    println("Parsed Buffer: \n")
    parser.getWordBuffer.foreach(x => x.print())

    val tokenizer = Tokenizer()
    val tokenBuffer = tokenizer.tokenize(parser.getWordBuffer)
    println("Token Buffer: \n")
    tokenBuffer.foreach(x => x.printID())

    val variables = ListBuffer[VariableHandler]()

    var index = 0
    while index < tokenBuffer.length do
        if tokenBuffer(index).id == Token.ID.Int_Type then

            var variableName = String()
            var variableType = VariableType.Types.None

            if tokenBuffer(index).id == Token.ID.Int_Type then
                variableType = VariableType.Types.Int_Type
            if tokenBuffer(index).id == Token.ID.String_Type then
                variableType = VariableType.Types.String_Type



            if tokenBuffer(index+1).id == Token.ID.NamedString then
                // Got Name Of Variable
                variableName = tokenBuffer(index + 1).string
                index += 1
            else
                // Did Not Get Name Of Variable
                println("Initialization Error!")


            if tokenBuffer(index).id == Token.ID.Equals then
                index += 1
                println(s"${tokenBuffer(index).string}")
                variables += VariableHandler(variableName, variableType)

        index += 1

    variables.foreach(x => x.printValue())







}