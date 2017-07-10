val list = new java.util.ArrayList
list->add(1)
list->add(2)
assertResult([1, 2])(list :> List<Int>)
val buffer = new java.lang.StringBuffer
buffer->append("A")->append("B")->append("C")
assertResult("ABC")(buffer->toString)
val a = ["FOO", "BAR", "BAZ"]
val b = [
  "FOO"
  "BAR"
  "BAZ"
]
assertResult(a)(b)
assertResult("F")("FOO"->substring(0, 1))
