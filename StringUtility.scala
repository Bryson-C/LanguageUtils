package com.bc.LanguageUtility

object StringUtility:
    def stringIsDigit(str: String): Boolean = {
        for chr <- str do
            if !chr.isDigit then
                return false
        true
    }
    def stringIsString(str: String, searchIterations: Int = 10000): Boolean = {
        val endChar = if str.charAt(0) == '\"' then '\"' else if str.charAt(0) == '\'' then '\'' else return false
        var index = 1
        while str.charAt(index) != endChar || index < searchIterations do
            if index >= searchIterations then return false
            index += 1
        true
    }
    def pascalCase(str: String): String = {
        var result = String()
        str.split(' ').foreach(x => {result += x.capitalize + " "})
        result
    }
