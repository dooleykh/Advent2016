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
        val rooms = openFile()
                    .map(line => {
                        val temp = line.split('[')
                        val checksum = temp(1).replace("]", "")
                        val temp2 = temp(0).split('-')
                        val id = temp2.last.toInt
                        (temp2.dropRight(1).mkString("-"), id, checksum)})
        val validRooms = filterRooms(rooms)
        println(validRooms.foldLeft(0) {(acc, room) => acc + room._2}) //Part 1

        validRooms.find(room => decrypt(room._1, room._2) == "northpole object storage") match {
            case Some(room) => println(room._2)
            case None => println("No location found")
        }
    }

    def filterRooms(rooms: Array[(String, Int, String)]) : Array[(String, Int, String)] = {
        rooms.filter(room => {
            var charCount = Map[Char, Int]()
            room._1.foreach(c => {
                if (c != '-') charCount = charCount + (c -> (charCount.getOrElse(c, 0) + 1))
            })
            val checksumCand = charCount.foldLeft(Map[Int, String]()) {(occurrances, kv) => {
                                            occurrances + (kv._2 -> (occurrances.getOrElse(kv._2, "") + kv._1.toString))
                                        }}
                                        .toList
                                        .sortWith((x, y) => y._1 < x._1) //sort with a negated comparator
                                        .map(_._2.sorted)
                                        .mkString("")
                                        .take(5)
            room._3 == checksumCand}
        )
    }

    def decrypt(encrypted: String, offset: Int) : String = {
        val alphabet = 'a' to 'z'
        var decrypted = ""
        encrypted.foreach(c =>
            if (c == '-') decrypted += ' '
            else decrypted += alphabet((alphabet.indexOf(c) + offset) % 26))
        decrypted
    }
}