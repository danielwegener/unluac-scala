package unluac.decompile

case class Output(var out: OutputProvider) {
  private var indentationLevel: Int = 0
  private var position: Int = 0

  def this()  {
    this(new OutputProvider() {
      override def print(s: String) {
        System.out.print(s)
      }
      override def println() = System.out.println()
    })
  }


  def indent() {
    indentationLevel += 2
  }

  def dedent() {
    indentationLevel -= 2
  }

  def getIndentationLevel: Int = {
    indentationLevel
  }

  def getPosition: Int = {
    position
  }

  def setIndentationLevel(indentationLevel: Int) {
    this.indentationLevel = indentationLevel
  }

  private def start() {
    if (position == 0) {
      {
        var i: Int = indentationLevel
        while (i != 0) {
            out.print(" ")
            position += 1
            i -= 1
        }
      }
    }
  }

  def print(s: String) {
    start()
    out.print(s)
    position += s.length
  }

  def println() {
    start()
    out.println()
    position = 0
  }

  def println(s: String) {
    print(s)
    println()
  }
}