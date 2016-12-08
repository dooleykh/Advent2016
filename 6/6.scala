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
        var lines = openFile()
        var columns = Array("", "", "", "", "", "", "", "")
        lines.foreach(line => 
            line.zip(0 to 7).foreach(pair => columns(pair._2) += pair._1)
        )

        //println(columns.map(_.groupBy(identity).mapValues(_.size).maxBy(_._2)._1).mkString) //Part 1
        println(columns.map(_.groupBy(identity).mapValues(_.size).minBy(_._2)._1).mkString) //Part 2
    }
}