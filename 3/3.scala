import scala.io.Source
object Day2 {
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
        //part1()
        part2()
    }

    def part1() : Unit = {
        val candidates = openFile();
        var valid = 0
        candidates.foreach {candidate =>
            val sides = candidate.split(" +").filterNot(_ == "").map(_.toInt)
            if (sides(0) + sides(1) > sides(2) && sides(0) + sides(2) > sides(1) && sides(1) + sides(2) > sides(0)) valid += 1
        }
        println(valid)
    }

    def part2() : Unit = {
        val nums = openFile().map(l => l.split(" +").filterNot(n => n == "").map(_.toInt))
        val candidates = (nums.map(_(0)) ++ nums.map(_(1)) ++ nums.map(_(2))).grouped(3)
        var valid = 0
        while (candidates.hasNext) {
            val sides = candidates.next()
            if (sides(0) + sides(1) > sides(2) && sides(0) + sides(2) > sides(1) && sides(1) + sides(2) > sides(0)) valid += 1            
        }
        println(valid)
    }
}