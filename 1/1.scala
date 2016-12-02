object Day1 {
    def main(args: Array[String]): Unit = {
        val route = "L5, R1, L5, L1, R5, R1, R1, L4, L1, L3, R2, R4, L4, L1, L1, R2, R4, R3, L1, R4, L4, L5, L4, R4, L5, R1, R5, L2, R1, R3, L2, L4, L4, R1, L192, R5, R1, R4, L5, L4, R5, L1, L1, R48, R5, R5, L2, R4, R4, R1, R3, L1, L4, L5, R1, L4, L2, L5, R5, L2, R74, R4, L1, R188, R5, L4, L2, R5, R2, L4, R4, R3, R3, R2, R1, L3, L2, L5, L5, L2, L1, R1, R5, R4, L3, R5, L1, L3, R4, L1, L3, L2, R1, R3, R2, R5, L3, L1, L1, R5, L4, L5, R5, R2, L5, R2, L1, L5, L3, L5, L5, L1, R1, L4, L3, L1, R2, R5, L1, L3, R4, R5, L4, L1, R5, L1, R5, R5, R5, R2, R1, R2, L5, L5, L5, R4, L5, L4, L4, R5, L2, R1, R5, L1, L5, R4, L3, R4, L2, R3, R3, R3, L2, L2, L2, L1, L4, R3, L4, L2, R2, R5, L1, R2"
        val directions = route.replace(" ", "").split(",").map(d => (d.charAt(0), d.substring(1, d.length).toInt))
        var (x, y, distance, dupDistance) = (0, 0, 0, -1)
        var foundDup = false
        var orientation = (0, 1)
        var locations = Set[(Int, Int)]()
        directions.foreach { dir =>
            var (turn, distance) = dir
            orientation = turn match {
                case 'R' => (orientation._2, -orientation._1)
                case 'L' => (-orientation._2, orientation._1)            
            }
            1 to distance foreach { _ =>
                x += orientation._1
                y += orientation._2
                distance = Math.abs(x) + Math.abs(y)
                if (locations((x, y)) && !foundDup) {
                    dupDistance = distance
                    foundDup = true
                }
                else
                    locations += ((x, y))
            }
        }
        println(x, y, distance, dupDistance)
    }
}