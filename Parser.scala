package com.bc.helloworld

import java.nio.file.{FileSystemException, Paths, Files}
import scala.collection.mutable.ListBuffer
import scala.io.Source.fromFile
import util.control.Breaks.*


class ParsedString(val string: String, val line: Int = -1, val column: Int = -1, val file: String = "Unknown"):
    def errorString(): String = s"[$file:$line:$column] '$string'"
    def print(): Unit = println(errorString())
    def errorStringLoc(): String = s"[$line:$column] '$string'"
    def printLoc(): Unit = println(errorStringLoc())


class ParserSettings(val path: String, val recordNewLines: Boolean = false, val log: Boolean = true, val logWithPath: Boolean = false):
    def this(path: String) = this(path, false, true, false)

class Parser:
    private var wordBuffer = ListBuffer[ParsedString]()
    def getWordBuffer: ListBuffer[ParsedString] = wordBuffer

    private def pushBufferIfNotEmpty(buffer: String, line: Int = -1, column: Int = -1, file: String = "Unknown"): String = {
        if buffer.nonEmpty then
            wordBuffer += ParsedString(buffer, line, column, file)
            ""
        else
            buffer
    }


    def parse(parserSettings: ParserSettings): Unit = {
        val path = parserSettings.path
        val source = fromFile(path)
        try {
            for line <- source.getLines().zipWithIndex do
                var index = 0
                var buffer = String()
                    while index < line._1.length do
                        breakable {
                            val current = line._1.charAt(index)
                            val nextChar: Option[Char] = if index < line._1.length - 1 then Some(line._1.charAt(index + 1)) else None
                            var lastChar: Option[Char] = if index > 0 then Some(line._1.charAt(index - 1)) else None

                            println(s"Buffer: $buffer")

                            if current == '\"' || current == '\'' then
                                val stringEnd = if current == '\"' then '\"' else '\''
                                buffer = pushBufferIfNotEmpty(buffer, line._2, index-buffer.length, path)
                                while line._1.charAt(index) != stringEnd do
                                    buffer += line._1.charAt(index)
                                    index += 1
                                buffer = pushBufferIfNotEmpty(buffer, line._2, index-buffer.length, path)
                                index += 1
                                break

                                if current.isWhitespace then
                                    buffer = pushBufferIfNotEmpty(buffer, line._2, index-buffer.length, path)
                                index += 1
                                break

                            else if current.isSpaceChar && parserSettings.recordNewLines then
                                buffer += '\n'
                                index += 1
                                break

                            else if current.isDigit then
                                buffer = pushBufferIfNotEmpty(buffer, line._2, index-buffer.length, path)
                                while line._1.charAt(index).isDigit do
                                    buffer += line._1.charAt(index)
                                    index += 1
                                buffer = pushBufferIfNotEmpty(buffer, line._2, index-buffer.length, path)
                                index += 1
                                break



                            buffer += current

                            index += 1
                        }
                    buffer = pushBufferIfNotEmpty(buffer, line._2, index-buffer.length, path)
            wordBuffer.foreach(x => if parserSettings.logWithPath then x.print() else x.printLoc())

            /*
            for i <- source.getLines() do
                var index = 0
                breakable {
                    for chr <- i.toCharArray do
                        if index > i.length then
                            break

                        // Check If Next Character Is Valid
                        val nextChar: Option[Char] =
                            if index < i.length-1 then
                                Some(i.charAt(index + 1))
                            else
                                None



                        // Check For Strings
                        if chr == '\"' || chr == '\'' then
                            buffer = pushBufferIfNotEmpty(buffer, line, index-buffer.length, path)
                            index += 1

                            val endChar = if chr == '\"' then '\"' else '\''

                            while i.charAt(index) != endChar do
                                buffer += i.charAt(index)
                                index += 1

                            index += 1

                            println(s"String '$buffer'")
                            buffer = s"$endChar$buffer$endChar"
                            wordBuffer += ParsedString(buffer, line, index-buffer.length, path)
                            buffer = ""

                        // Check If A Number
                        else if chr.isDigit then
                            buffer = pushBufferIfNotEmpty(buffer, line, index-buffer.length, path)
                            while i.charAt(index).isDigit do
                                buffer += i.charAt(index)
                                index += 1



                            index += 1
                            wordBuffer += ParsedString(buffer, line, index-buffer.length, path)
                            buffer = ""

                        // Split On Spaces
                        else if chr.isSpaceChar then
                            if chr.isWhitespace then
                                wordBuffer += ParsedString(buffer, line, index-buffer.length, path)
                                buffer = ""
                            else
                                wordBuffer += ParsedString(chr.toString, line, index, path)


                        else if chr == '\n' || chr == '\t' && recordNewLines then
                            wordBuffer += ParsedString(chr.toString, line, index, path)


                        // Check For Special Character/Operators
                        else if !chr.isSpaceChar && !chr.isLetterOrDigit then
                            buffer = pushBufferIfNotEmpty(buffer, line, index-buffer.length, path)
                            wordBuffer += ParsedString(chr.toString, line, index, path)
                       /*     println(s"Chr: $chr | Next Chr: ${nextChar.get}")

                            buffer = if nextChar.nonEmpty && !nextChar.get.isSpaceChar then s"$chr${nextChar.get}" else chr.toString
                            if buffer == "//" then
                                buffer = ""
                                while index < i.length do
                                    print(i.charAt(index))
                                    index += 1
                                break

                            println(s"Buffer: $chr")
                            wordBuffer += ParsedString(buffer, line, index, path)
                        */


                        // Check If Line Ends
                        else if index == i.toCharArray.length then
                            wordBuffer += ParsedString(buffer, line, index-buffer.length, path)
                            // Put New Line If Setting
                            if recordNewLines then
                                wordBuffer += ParsedString("\n", line, i.length-1, path)
                            buffer = ""


                        if index < i.length && i.charAt(index) != ' ' then
                            buffer += i.charAt(index)
                        lastChar = chr
                        index += 1

                    buffer = pushBufferIfNotEmpty(buffer, line, index-buffer.length, path)
                }
                println()
                line += 1
            */
        } catch {
            case strErr: StringIndexOutOfBoundsException => println(s"String Index Error! '$strErr'")
            case fileErr: FileSystemException => println(s"File Error Occurred! '$fileErr'")
            case err: Throwable => println(s"Error Occurred! '$err'")
        } finally {
            source.close()
        }

    }



