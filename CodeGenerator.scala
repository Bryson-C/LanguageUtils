package com.bc.LanguageUtility


class CCodeGenerator:
    def function(name: String, retType: String = "void", arguments: List[(String, String)] = List()): String = {
        var functionString = s"$retType $name("
        for i <- arguments.zipWithIndex do
            functionString += s"${i._1._2} ${i._1._1}"
            if i._2 < arguments.length-1 then
                functionString += ", "
        functionString += ")"
        functionString
    }

object CodeGenerator:
    def targetC: CCodeGenerator = CCodeGenerator()
