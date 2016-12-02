import scala.io.Source
object Day2 {
    def openFile() : Array[String] = {
        var lines = Array[String]()
        val buffered = Source.fromFile("input.txt")
        for (line <- buffered.getLines) {
            lines = lines :+ line
        }
        buffered.close
        lines
    }

    def main(args: Array[String]) : Unit = {
        val instructions = openFile()
        var code = ""
        //Part 1 keypad
        // val validMoves =
        //     Map(
        //         '1' -> Map('R' -> '2', 'D' -> '4'),
        //         '2' -> Map('L' -> '1', 'R' -> '3', 'D' -> '5'),
        //         '3' -> Map('L' -> '2', 'D' -> '6'),
        //         '4' -> Map('R' -> '5', 'U' -> '1', 'D' -> '7'),
        //         '5' -> Map('L' -> '4', 'R' -> '6', 'U' -> '2', 'D' -> '8'),
        //         '6' -> Map('L' -> '5', 'U' -> '3', 'D' -> '9'),
        //         '7' -> Map('R' -> '8', 'U' -> '4'),
        //         '8' -> Map('L' -> '7', 'R' -> '9', 'U' -> '5'),
        //         '9' -> Map('L' -> '8', 'U' -> '6')
        //     )
        
        //Part 2 keypad
        val validMoves = 
            Map(
                '1' -> Map('D' -> '3'),
                '2' -> Map('R' -> '3', 'D' -> '6'),
                '3' -> Map('L' -> '2', 'R' -> '4', 'U' -> '1', 'D' -> '7'),
                '4' -> Map('L' -> '3', 'D' -> '8'),
                '5' -> Map('R' -> '6'),
                '6' -> Map('L' -> '5', 'R' -> '7', 'U' -> '2', 'D' -> 'A'),
                '7' -> Map('L' -> '6', 'R' -> '8', 'U' -> '3', 'D' -> 'B'),
                '8' -> Map('L' -> '7', 'R' -> '9', 'U' -> '4', 'D' -> 'C'),
                '9' -> Map('L' -> '8'),
                'A' -> Map('R' -> 'B', 'U' -> '6'),
                'B' -> Map('L' -> 'A', 'R' -> 'C', 'U' -> '7', 'D' -> 'D'),
                'C' -> Map('L' -> 'B', 'U' -> '8'),
                'D' -> Map('U' -> 'B')
            )
        var position = '5'
        instructions.foreach { instruction =>
            instruction.foreach { move =>
                position =  validMoves(position).getOrElse(move, position)
            }
            code += position.toString
        }
        println(code)
    }
}