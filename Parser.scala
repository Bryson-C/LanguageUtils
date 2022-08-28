package com.bc.helloworld

import scala.collection.mutable.ListBuffer
import scala.io.Source.fromFile

class ParsedString(val string: String, val line: Int = -1, val column: Int = -1, file: String = "Unknown"):
    def print(): Unit = println(s"[$file:$line:$column] $string")

class Parser:
    private var wordBuffer = ListBuffer[ParsedString]()

    def parse(path: String): Unit = {
        val source = fromFile(path);
        try {
            var line = 1;
            for string <- source.getLines() do {
                val row = string.split(' ')

                for i <- row do {
                    wordBuffer += ParsedString(string, line, i.length, path)
                }

                line += 1
            }
            wordBuffer.foreach(x => x.print())
        } catch {
            case _: Throwable => println("File Error Occurred!")
        } finally {
            source.close()
        }
    }

