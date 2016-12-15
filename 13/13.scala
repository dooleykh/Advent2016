object Day13 {
    def main(args: Array[String]) : Unit = {
        part1()
        part2()
    }

    def part1() : Unit = {
        var currentPos = ((1, 1), 0)
        var computed = Set(currentPos._1)
        var queue = collection.mutable.Queue(currentPos)
        while (queue.nonEmpty && (queue.head)._1 != (31, 39)) {
            currentPos = queue.dequeue
            val nextPos = validPositions(currentPos._1).filter(!computed(_))
            for (pos <- nextPos) {
                queue.enqueue((pos, currentPos._2 + 1))
                computed = computed + pos
            }
        }
        println(queue.head)
    }

    def part2() : Unit = {
        var currentPos = ((1, 1), 0)
        var computed = Set(currentPos._1)
        var queue = collection.mutable.Queue(currentPos)
        while (queue.nonEmpty) {
            currentPos = queue.dequeue
            if (currentPos._2 != 50) {
                val nextPos = validPositions(currentPos._1).filter(!computed(_))
                for (pos <- nextPos) {
                    queue.enqueue((pos, currentPos._2 + 1))
                    computed = computed + pos
                }
            }
        }
        println(computed.size)
    }

    def validPositions(pos: (Int, Int)) : List[(Int, Int)] = {
        val (x, y) = pos
        List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
            .filter(_._1 >= 0)
            .filter(_._2 >= 0)
            .filter(isOpen)
    }

    def isOpen(pos: (Int, Int)) : Boolean = {
        val favoriteNumber = 1362
        val (x, y) = pos
        var candidate = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber
        var sum = 0
        while (candidate != 0) {
            sum += (candidate & 1)
            candidate = candidate >> 1
        }
        sum % 2 == 0
    }
}