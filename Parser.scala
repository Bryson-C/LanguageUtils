package com.bc.helloworld

import scala.collection.mutable.ListBuffer
import scala.io.Source.fromFile
import util.control.Breaks._

class ParsedString(val string: String, val line: Int = -1, val column: Int = -1, file: String = "Unknown"):
    def print(): Unit = println(s"[$file:$line:$column] $string")

class Parser:
    private var wordBuffer = ListBuffer[ParsedString]()
    def getWordBuffer: ListBuffer[ParsedString] = return wordBuffer

    def parse(path: String): Unit = {
        val source = fromFile(path);
        try {
            var buffer = String()
            var lastChar: Char = ' '
            var line = 0
            for i <- source.getLines() do
                breakable {
                    for chr <- i.toCharArray do
                        print(s"chr: '$chr', ")
                        buffer += chr
                        lastChar = chr
                }
                println()
                wordBuffer += ParsedString(buffer, line)
                buffer = ""
                line += 1


        } catch {
            case x: Throwable => println(s"File Error Occurred! '$x'")
        } finally {
            source.close()
        }

        wordBuffer.foreach(x => x.print())

    }



