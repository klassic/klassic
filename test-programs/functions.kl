def myFoldLeft(list) = (z) => (f) => {
  if(isEmpty(list)) z else myFoldLeft(tail(list))(f(z, head(list)))(f)
}
assertResult(10)(myFoldLeft([1 2 3 4])(0)((x, y) => x + y))
assertResult("ABC")(myFoldLeft(["A" "B" "C"])("")((x, y) => x + y))
assertResult([4 3 2 1])(myFoldLeft([1 2 3 4])([])((x, y) => y #cons x))
assertResult([4 3 2 1])(myFoldLeft([1 2 3 4])([]){x, y => y #cons x})

val sum = myFoldLeft([1 2 3 4 5])(0){x, y => x + y}
assertResult(15)(sum)
assertResult(10.0)(foldLeft([1.0 2.0 3.0 4.0])(0.0){x, y => x + y})
