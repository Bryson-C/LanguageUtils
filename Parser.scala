package com.bc.LanguageUtility


import java.nio.file.{FileSystemException, Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.io.Source.fromFile
import scala.util.control.Breaks.*

object FileUtility:
    def openFile(path: String): scala.io.Source = fromFile(path)
    def readFile(path: String): String = {
        val source = fromFile(path)
        var buffer = String()
        try {
            val lines = for i <- source.getLines() yield i
            buffer = lines.toString()
        } catch {
            case err: Throwable => println(s"Error: $err")
        } finally {
            source.close()
        }
        buffer
    }
    def readFileAsLines(path: String): ListBuffer[String] = {
        val source = fromFile(path)
        var buffer = ListBuffer[String]()
        try {
            val lines = for i <- source.getLines() yield i
            lines.foreach(x => buffer += x)
        } catch {
            case err: Throwable => println(s"Error: $err")
        } finally {
            source.close()
        }
        buffer
    }

class ParsedString(val string: String, val line: Int = -1, val column: Int = -1, val file: String = "Unknown"):
    def errorString(): String = s"[$file:$line:$column] '$string'"
    def print(): Unit = println(errorString())
    def errorStringLoc(): String = s"[$line:$column] '$string'"
    def printLoc(): Unit = println(errorStringLoc())


class ParserSettings(val path: String, val recordNewLines: Boolean = false, val log: Boolean = true, val logWithPath: Boolean = false):
    def this(path: String) = this(path, false, true, false)

class Parser:
    private val wordBuffer = ListBuffer[ParsedString]()
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
        val content = FileUtility.readFileAsLines(path)

        for line <- content.zipWithIndex do
            var index = 0
            var buffer = String()
                while index < line._1.length do
                    breakable {
                        val current = line._1.charAt(index)
                        val nextChar: Option[Char] = if index < line._1.length - 1 then Some(line._1.charAt(index + 1)) else None
                        var lastChar: Option[Char] = if index > 0 then Some(line._1.charAt(index - 1)) else None

                        if current == '\"' || current == '\'' then
                            val stringEnd = if current == '\"' then '\"' else '\''
                            buffer = pushBufferIfNotEmpty(buffer, line._2, index-buffer.length, path)
                            index += 1
                            while line._1.charAt(index) != stringEnd do
                                buffer += line._1.charAt(index)
                                index += 1
                            buffer = s"\"$buffer\""
                            buffer = pushBufferIfNotEmpty(buffer, line._2, index-buffer.length+2, path)
                            index += 1
                            break

                        if current.isWhitespace || current == ' ' then
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

                        else if !current.isSpaceChar && !current.isLetterOrDigit then
                            buffer = pushBufferIfNotEmpty(buffer, line._2, index, path)
                            if nextChar.nonEmpty && current == nextChar.get then
                                buffer = s"$current$current"
                                index += 1
                            else
                                buffer += current
                            buffer = pushBufferIfNotEmpty(buffer, line._2, index, path)
                            index += 1
                            break


                        buffer += current

                        index += 1
                    }
                buffer = pushBufferIfNotEmpty(buffer, line._2, index-buffer.length, path)
        wordBuffer.foreach(x => if parserSettings.logWithPath then x.print() else x.printLoc())

    }


