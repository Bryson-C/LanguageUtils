package com.bc.LanguageUtility

object StringUtility:
    def stringIsDigit(str: String): Boolean = {
        for chr <- str do
            if !chr.isDigit then
                return false
        true
    }
