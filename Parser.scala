package com.bc.helloworld

import scala.collection.mutable.ListBuffer
import scala.io.Source.fromFile
import util.control.Breaks._

class ParsedString(val string: String, val line: Int = -1, val column: Int = -1, file: String = "Unknown"):
    def print(): Unit = println(s"[$file:$line:$column] $string")

class Parser:
    private var wordBuffer = ListBuffer[ParsedString]()

    def parse(path: String): Unit = {
        val source = fromFile(path);
        try {
            var line = 1;
            var buffer = String();
            for wrd <- source.getLines() do {
                for (i <- 0 until wrd.length) do {
                    if i+1 < wrd.length then
                        if wrd.charAt(i) == '/' && wrd.charAt(i+1) == '/' then
                            println(s"[$line:$i:\"Unknown\"] Comment")
                }

                //wordBuffer += ParsedString(i, line, i.length, path)


                line += 1
            }
            //wordBuffer.foreach(x => x.print())
        } catch {
            case x: Throwable => println(s"File Error Occurred! '$x'")
        } finally {
            source.close()
        }
    }

