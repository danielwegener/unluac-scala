package unluac.test;

public class TestFiles {

  public static final String[] tests = {
    "assign",
    "literal",
    "multiassign",
    "expression",
    "functioncall",
    "literallist",
    "multiliteraltarget",
    "closure",
    "ifthen",
    "condition",
    "nestedif",
    "nestedif02",
    "ifthenelse",
    "while",
    "repeat",
    "booleanassign01",
    "booleanassign02",
    "booleanassign03",
    "booleanassign04",
    "booleanassign05",
    "booleanassign06",
    "booleanassign07",
    "booleanassign08",
    "booleanassign09",
    "booleanassign10",
    "booleanselfassign01",
    "booleanexpression01",
    "booleanexpression02",
    "booleanexpression03",
    "booleanexpression04",
    "booleanexpression05",
    "booleanmultiassign01",
    "compareassign01",
    "compareexpression",
    "combinebexpression01",
    "combinebexpression02",
    "combinebexpression03",
    "combinebexpression04",
    "complexassign01",
    "complexassign02",
    "complexassign03",
    "compareorder01",
    "compareorder02",
    "compareorder03",
    "compareorder04",
    "compareorder05",
    "ellipsis",
    "table01",
    "table02",
    "localfunction01",
    "localfunction02",
    "localfunction03",
    "localfunction04",
    "declare",
    "declare02",
    "declare03",
    "adjust01",
    "adjust02",
    "adjust03",
    "adjust04",
    "adjust05",
    "final01",
    "final02",
    "doend01",
    "doend02",
    "doend03",
    "doend04",
    "doend05",
    "control01",
    "control02",
    "control03",
    "control04",
    "control05",
    "control06",
    "method01",
    "inlinefunction01",
    "report01a",
    "report01b",
    "report01c",
    "report01d",
    "report01_full"
  };
  
  public static TestSuite suite = new TestSuite(".\\test\\src\\", tests);
  
}