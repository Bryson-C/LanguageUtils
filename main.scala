package com.bc.LanguageUtility

// TODO: Add Documentation For LanguageUtility Package
import com.bc.LanguageUtility._



@main def main(): Unit = {

    val settings = ParserSettings("D:\\Scala\\src\\main\\data.lang", true)
    val parser = Parser();
    parser.parse(settings)
    parser.getWordBuffer.foreach(x => x.print())

    val tokenizer = Tokenizer()
    tokenizer.tokenize(parser.getWordBuffer)

}