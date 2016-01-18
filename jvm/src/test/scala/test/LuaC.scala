package test

import java.io.{BufferedReader, IOException, InputStreamReader}

object LuaC {
  @throws(classOf[IOException])
  def compile(in: String, out: String) {
    var luac:String = ""
    if(System.getProperty("os.name").contains("Windows")) {
      luac = "luac.exe"
    } else {
      luac = "luac"
    }
    val pb = new ProcessBuilder(luac, "-o", out, in)
    pb.directory(null)
    val p: Process = pb.start
    while (true) {
      try {
        if (p.waitFor == 0) {
          return
        }
        else {
          val r: BufferedReader = new BufferedReader(new InputStreamReader(p.getErrorStream))
          var line: String = null
          do {
            line = r.readLine
            if (line != null) {
              System.err.println(line)
            }
          } while (line != null)
          throw new IOException("luac failed on file: " + in)
        }
      }
      catch {
        case e: InterruptedException =>
      }
    }
  }
}