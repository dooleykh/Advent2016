import java.security.MessageDigest

object Day14 {
    def main(args: Array[String]) : Unit = {
        val md5 = MessageDigest.getInstance("MD5")
        var hashes = collection.mutable.ArrayBuffer[String]()
        var keys = collection.mutable.ArrayBuffer[String]()
        val input = "yjdafjpo"
        var candidate = ""
        var i = 0
        for (c <- 0 to 999) {
            candidate = input + c.toInt
            hashes += getHash(candidate, md5)
        }
        i = 0
        while(keys.length < 64) {
            hashes += getHash(input + (i + 1000).toString, md5)
            hashes(i).sliding(3).find(s => s(0) == s(1) && s(0) == s(2)) match {
                case Some(repeated) => {
                    val c = (1 to 5).map(_ => repeated(0)).mkString
                    if ((i + 1 to i + 1000).exists(j => hashes(j).contains(c))) {
                        keys += hashes(i)
                        println(i)
                    }
                }
                case None => {}
            }

            i += 1
        }
    }

    //Part 1
    // def getHash(plainText: String, digest: MessageDigest) : String = {
    //     digest.digest(plainText.getBytes).map("%02X".format(_)).mkString.toLowerCase
    // }

    //Part 2
    def getHash(plainText: String, digest: MessageDigest) : String = {
        var ret = digest.digest(plainText.getBytes).map("%02X".format(_)).mkString.toLowerCase
        for (_ <- 1 to 2016) ret = digest.digest(ret.getBytes).map("%02X".format(_)).mkString.toLowerCase
        ret
    }
}