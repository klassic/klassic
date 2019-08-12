// map
assertResult([2 3 4])(map([1 2 3])((x) => x + 1))
assertResult([3 4 5])(map([2 3 4]){x => x + 1})

// head
assertResult(1)(head([1 2 3 4]))

// tail
assertResult([2 3 4])(tail([1 2 3 4]))

// cons
assertResult([1 2 3 4])(cons(1)([2 3 4]))

// size
assertResult(5)(size([1 2 3 4 5]))

// isEmpty
assertResult(true)(isEmpty([]))
assertResult(false)(isEmpty([1 2 3]))

// foldLeft
assertResult(10)(foldLeft([1 2 3 4])(0)((x, y) => x + y))
assertResult(10.0)(foldLeft([1.0 2.0 3.0 4.0])(0.0){x, y => x + y})
assertResult(24.0)(foldLeft([1.0 2.0 3.0 4.0])(1.0){x, y => x * y})
