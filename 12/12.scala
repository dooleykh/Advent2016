import scala.io.Source
object Day6 {
    def openFile() : Array[String] = {
        var lines = Array[String]()
        val buffered = Source.fromFile("input.txt")
        for (line <- buffered.getLines) {
            lines = lines :+ line.trim
        }
        buffered.close
        lines
    }

    def main(args: Array[String]) : Unit = {
        val instructions = openFile()
        var i = 0;
        //var registers = collection.mutable.Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0) //Part 1
        var registers = collection.mutable.Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0) //Part 2
        while (i < instructions.length) {
            val instruction = instructions(i).split(" ")
            val workingValue = instruction(1)
            instruction match {
                case Array("cpy", _*) => {
                    val copyValue = if (registers.contains(workingValue)) registers(workingValue) else workingValue.toInt
                    registers(instruction(2)) = copyValue
                    i += 1
                }
                case Array("inc", _*) => {
                    registers(workingValue) += 1 
                    i += 1
                }
                case Array("dec", _*) => {
                    registers(workingValue) -= 1                     
                    i += 1
                }
                case Array("jnz", _*) => {
                    val compValue = if (registers.contains(workingValue)) registers(workingValue) else workingValue.toInt               
                    if (compValue != 0) i += instruction(2).toInt else i += 1
                }
            }
        }
        println(registers.toList)
    }
}