package unluac.parse

case class LFunction(
                      header: BHeader,
                      code: Array[Int],
                      locals: Array[LLocal],
                      constants: Array[LObject],
                      upvalues: Array[LUpvalue],
                      functions: Array[LFunction],
                      maximumStackSize: Int,
                      numUpValues: Int,
                      numParams: Int,
                      vararg: Int
                    ) extends BObject