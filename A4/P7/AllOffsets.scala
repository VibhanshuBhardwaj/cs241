import java.io._

object AllOffsets {
	def main(args: Array[String]) : Unit = {
		val pw = new PrintWriter(new File("allOffsets.asm" ))

		for (i <- (-32768 to 32767 by 1)) {
			pw.write("lw $1, " + i +"($2)" + "\n")
		}
		//var j : UShort = 0
		for (j <- (0 to 0xffff)) {
			pw.write("sw $1, " + "0x" +j.toHexString +"($2)" + "\n")
		}
		
		pw.close();
			
	}
}
