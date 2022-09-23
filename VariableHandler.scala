package com.bc.LanguageUtility

object VariableType:
    enum Types:
        case None, Int_Type, String_Type

class VariableHandler(val name: String, val _type: VariableType.Types):
    var stringArray: Array[String] = Array[String]()
    var integerArray: Array[Int] = Array[Int]()

    def this(name: String, _type: VariableType.Types, value: Array[Int]) = {
        this(name, _type)
        integerArray = value
    }
    def this(name: String, _type: VariableType.Types, value: Array[String]) = {
        this(name, _type)
        stringArray = value
    }
    def this() = this("_", VariableType.Types.None)
    def printValue(): Unit = {
        print(s"Var $name of Type $_type")
        println(s"${
            _type match
                case(VariableType.Types.None) => println("No Value")
                case(VariableType.Types.Int_Type) => println(integerArray.mkString("[",",","]"))
                case(VariableType.Types.String_Type) => println(stringArray.mkString("[",",","]"))
        }")
    }
