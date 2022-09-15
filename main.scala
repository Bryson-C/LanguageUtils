package com.bc.LanguageUtility

// TODO: Add Documentation For LanguageUtility Package
import com.bc.LanguageUtility.*

import scala.collection.mutable.ListBuffer



@main def main(): Unit = {

    val settings = ParserSettings("D:\\Scala\\src\\main\\data.lang", true)
    val parser = Parser();
    parser.parse(settings)
    parser.getWordBuffer.foreach(x => x.print())

    val tokenizer = Tokenizer()
    val tokenBuffer = tokenizer.tokenize(parser.getWordBuffer)
    tokenBuffer.foreach(x => x.printID())



}