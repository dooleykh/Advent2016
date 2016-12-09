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
        //println(decompress1(input).length)
        println(decompress2(input))
    }

    def decompress1(message: String) : String = {
        var uncompressed = ""
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
                (1 to repetitions.toInt).foreach(r => {
                    uncompressed += message.substring(i, i + charCount.toInt)
                })
                i += charCount.toInt
            }
            else {
                uncompressed += message(i)
                i += 1
            }
        }
        return uncompressed
    }

    //Answer is 10964557606
    //TODO: Improve the performance of part 2 (took 10 minutes or so to run this time...)
    def decompress2(message: String) : BigInt = {
        var length = BigInt(0)
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
                var substring = ""
                (1 to repetitions.toInt).foreach(r => {
                    substring += message.substring(i, i + charCount.toInt)
                })
                length += decompress2(substring)
                i += charCount.toInt
            }
            else {
                length += 1
                i += 1
            }
        }
        length
    }
}