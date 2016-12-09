import scala.collection.mutable.ArrayBuffer
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
        val lines = openFile()
        var screen = ArrayBuffer.fill(6, 50)(false)
        for (line <- lines) {  
            val split = line.split(" ")
            split match {
                case Array("rect", _*) => {
                    val Array(width, height) = split(1).split("x").map(_.toInt)
                    (0 to height - 1).foreach(y => {
                        (0 to width - 1).foreach(x => screen(y)(x) = true)
                    })
                }
                case Array("rotate", "row", _*) => {
                    val row = split(2).split("=")(1).toInt
                    val amount = split(4).toInt

                    screen(row) = screen(row).takeRight(amount) ++ screen(row).dropRight(amount)
                }
                case Array("rotate", "column", _*) => {
                    val column = split(2).split("=")(1).toInt
                    val amount = split(4).toInt
                    val mappedColumn = screen.map(_(column))
                    val shiftedColumn = mappedColumn.takeRight(amount) ++ mappedColumn.dropRight(amount)

                    (0 to screen.length - 1).foreach(i => screen(i)(column) = shiftedColumn(i))
                }
            }
        }
        printScreen(screen)
        println
        println(screen.flatten.filter(x => x).length)
    }
    
    def printScreen(screen: ArrayBuffer[ArrayBuffer[Boolean]]) : Unit = {
        screen.foreach(row => {
            row.foreach(column => {
                if (column) print("#")
                else print(".")
            })
            println
        })
    }
}