package com.bc.helloworld


import com.bc.helloworld.Parser


extension (p: Parser)
    def ver(): Unit = println("Version 0.0.0");


@main def main(): Unit = {


    val parser = Parser();
    parser.parse("D:\\Scala\\src\\main\\data.txt")
    parser.ver()



}