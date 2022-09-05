package com.bc.LanguageUtility

import com.bc.LanguageUtility._



@main def main(): Unit = {

    var greeting = "Hello world, this is a parser. im figuring out language features!"
    println(greeting)
    greeting = StringUtility.pascalCase(greeting)
    println(greeting)


    val settings = ParserSettings("D:\\Scala\\src\\main\\data.txt")
    val parser = Parser();
    parser.parse(settings)

    val tokenizer = Tokenizer()
    tokenizer.tokenize(parser.getWordBuffer)

}