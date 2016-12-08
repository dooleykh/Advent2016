import scala.io.Source
object Day7 {
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
        val lines = openFile()
        part1(lines)
        part2(lines)
    }

    def part1(lines: Array[String]): Unit = {
        val filtered = lines.filter(line => {
            var brackets = Array[String]()
            var noBrackets = Array[String]("")
            var inBrackets = false
                for (c <- line) {
                    if (c == '[') {
                        brackets = brackets :+ ""
                        inBrackets = true
                    }
                    else if (c == ']') {
                        noBrackets = noBrackets :+ ""
                        inBrackets = false 
                    }
                    else if (!inBrackets) noBrackets(noBrackets.length - 1) += c
                    else brackets(noBrackets.length - 1) += c
                }
                val noBracketsABBA = noBrackets.exists(_.sliding(4).exists(s => s(0) != s(1) && (s(0) == s(3)) && (s(1)) == s(2)))
                val bracketsABBA = brackets.filter(_.sliding(4).exists(s => s(0) != s(1) && (s(0) == s(3)) && (s(1)) == s(2))).isEmpty
                noBracketsABBA && bracketsABBA
        })
        println(filtered.length)
    }

    def part2(lines: Array[String]): Unit = {
        val filtered = lines.filter(line => {
            var brackets = Array[String]()
            var noBrackets = Array[String]("")
            var inBrackets = false
                for (c <- line) {
                    if (c == '[') {
                        brackets = brackets :+ ""
                        inBrackets = true
                    }
                    else if (c == ']') {
                        noBrackets = noBrackets :+ ""
                        inBrackets = false 
                    }
                    else if (!inBrackets) noBrackets(noBrackets.length - 1) += c
                    else brackets(noBrackets.length - 1) += c
                }
                val abaKeys = noBrackets.map(_.sliding(3).filter(s => s(0) != s(1) && s(0) == s(2))).map(_.toList).flatten
                val candidates = abaKeys.map(k => "" + k(1) + k(0) + k(1))
                !abaKeys.isEmpty && brackets.exists(b => candidates.exists(c => b.contains(c)))
        })
        println(filtered.length)
    }
}