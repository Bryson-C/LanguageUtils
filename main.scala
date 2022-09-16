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

    val contextParser = Contextualization()
    val contextTokens = contextParser.contextualizeAll(tokenBuffer.toList)
    println("Context Buffer: \n")
    contextTokens.foreach(x => x.printID_())


}