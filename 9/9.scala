import scala.io.Source

object Day8 {
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
        val input = openFile().mkString
        println(decompress1(input)) //Part 1
        println(decompress2(input)) //Part 2
    }

    def decompress1(message: String) : Int = {
        var length = 0
        var i = 0
        while (i < message.length) {
            if (message(i) == '(') {
                var charCount = ""
                var repetitions = ""
                i += 1
                while (message(i) != 'x') {
                    charCount += message(i)
                    i += 1
                }
                i += 1
                while (message(i) != ')') {
                    repetitions += message(i)
                    i += 1
                }
                i += 1
                length += charCount.toInt * repetitions.toInt
                i += charCount.toInt
            }
            else {
                length += 1
                i += 1
            }
        }
        length
    }

    def decompress2(message: String) : Long = {
        var length = 0L
        var i = 0
        while (i < message.length) {
            if (message(i) == '(') {
                var charCount = ""
                var repetitions = ""
                i += 1
                while (message(i) != 'x') {
                    charCount += message(i)
                    i += 1
                }
                i += 1
                while (message(i) != ')') {
                    repetitions += message(i)
                    i += 1
                }
                val charCountInt = charCount.toInt
                val repetitionsInt = repetitions.toInt

                i += 1

                length += decompress2(message.substring(i, i + charCountInt)) * repetitionsInt
                i += charCountInt
            }
            else {
                length += 1
                i += 1
            }
        }
        length
    }
}