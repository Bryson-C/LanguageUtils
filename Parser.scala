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
            var line = 1
            for i <- source.getLines() do
                var index = 0
                breakable {
                    for chr <- i.toCharArray do
                        if index > i.length then
                            break

                        val nextChar: Option[Char] =
                            if index < i.length-1 then
                                Some(i.charAt(index + 1))
                            else
                                None

                        if chr == '/' && nextChar.get == '/' then
                            break


                        else if chr == '\"' then
                            index += 1
                            wordBuffer += ParsedString(buffer, line, index-buffer.length, path)
                            buffer = ""
                            while i.charAt(index) != '\"' do
                                buffer += i.charAt(index)
                                index += 1

                            index += 1

                            println(s"String '$buffer'")
                            wordBuffer += ParsedString(buffer, line, index-buffer.length, path)
                            buffer = ""

                        else if chr.isDigit then
                            wordBuffer += ParsedString(buffer, line, index-buffer.length, path)
                            buffer = ""
                            while chr.isDigit do
                                println(s"$buffer")
                                buffer += i.charAt(index)
                                index += 1

                            println(s"Integer '$buffer'")
                            wordBuffer += ParsedString(buffer, line, index-buffer.length, path)
                            buffer = ""

                        else if chr == ' ' then
                            wordBuffer += ParsedString(buffer, line, index-buffer.length, path)
                            buffer = ""

                        else if index == i.toCharArray.length then
                            wordBuffer += ParsedString(buffer, line, index-buffer.length, path)
                            buffer = ""


                        buffer += chr
                        lastChar = chr
                        index += 1

                    if buffer.nonEmpty then
                        wordBuffer += ParsedString(buffer, line, index, path)
                        buffer = ""
                }
                println()
                line += 1


        } catch {
            case strErr: StringIndexOutOfBoundsException => println(s"String Index Error! '$strErr'")
            case x: Throwable => println(s"File Error Occurred! '$x'")
        } finally {
            source.close()
        }

        wordBuffer.foreach(x => x.print())

    }



