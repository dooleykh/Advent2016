import java.security.MessageDigest

object Day5 {
    def main(args: Array[String]) : Unit = {
        part1()
        part2()
    }

    def part1() : Unit = {
        val doorID = "ugkcyxxp"
        val md5 = MessageDigest.getInstance("MD5")
        var password = ""
        var i = 0
        var hashedHex = Array[String]()
        while (password.length != 8) {
            hashedHex = md5.digest((doorID + i.toString).getBytes)
                           .take(3)
                           .map("%02X".format(_))
            if (hashedHex(0) == "00" && hashedHex(1) == "00" && hashedHex(2).startsWith("0")) {
                password += hashedHex(2).last
            }
            i += 1
        }
        println(password.toLowerCase)
    }

    def part2() : Unit = {
        val doorID = "ugkcyxxp"
        val md5 = MessageDigest.getInstance("MD5")
        var password = Array[String]("", "", "", "", "", "", "", "")
        var length = 0
        var length8 = false
        var i = 0
        var hashedHex = Array[String]()
        while (!length8) {
            hashedHex = md5.digest((doorID + i.toString).getBytes)
                           .take(4)
                           .map("%02X".format(_))
            if (hashedHex(0) == "00" && hashedHex(1) == "00" && hashedHex(2).startsWith("0")) {
                val posDigit = hashedHex(2).last.asDigit 
                if (hashedHex(2).last < '8' && password(posDigit) == "") {
                    password(posDigit) += hashedHex(3).head
                    length += 1
                    length8 = (length == 8)
                }
            }
            i += 1
        }
        println(password.mkString("").toLowerCase)
    }
}