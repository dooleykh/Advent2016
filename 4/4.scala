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
        val lines = openFile()
        var sumIds = 0
        for (line <- lines) {
            val temp = line.split('[')
            val temp2 = temp(0).split('-')

            val id = temp2.last.toInt
            var charCount = Map[Char, Int]()
            val checksum = temp(1).replace("]", "")
            for (substring <- temp2.dropRight(1)) {
                for (c <- substring) {
                    charCount = charCount + (c -> (charCount.getOrElse(c, 0) + 1))
                }
            }
            val checksumCand = charCount
                               .values
                               .toList
                               .sorted
                               .reverse
                               .take(5)
                               .distinct
                               .map(v => charCount.toList.filter(kv => kv._2 == v).map(kv => kv._1))
                               .map(characters => characters.sorted.mkString(""))
                               .mkString("")
                               .take(5)
            if (checksum == checksumCand) sumIds += id
        }
        println(sumIds)
    }
}