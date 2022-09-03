package com.bc.LanguageUtility

import com.bc.LanguageUtility._



@main def main(): Unit = {

    val settings = ParserSettings("D:\\Scala\\src\\main\\data.txt")
    val parser = Parser();
    parser.parse(settings)

    val tokenizer = Tokenizer()
    tokenizer.tokenize(parser.getWordBuffer)

}