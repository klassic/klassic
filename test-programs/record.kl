record Person {
  name: *
  age: Int
  profile: #Profile
}

record Profile {
  description: *
  hobby: *
}

record Stub {
  p: #GPoint<Int, Int>
}

record X1 {
  x2: #X2
}

record X2 {
  g: #GPoint<Int, Int>
}

record S {
  v: Int
  to_string: (*) => *
}

record GPoint<'x, 'y> {
  x: 'x
  y: 'y
}

record Pair <'a, 'b> {
  _1: 'a
  _2: 'b
}

val s = #S(100, (this) => {
  "#{(this :> #S).v}"
})

assert("100" == s.to_string())

val stub = #Stub(#GPoint(3, 5))
assertResult(3)(stub.p.x)
assertResult(5)(stub.p.y)

val x1 = #X1(#X2(#GPoint(4, 6)))
assertResult(4)(x1.x2.g.x)
assertResult(6)(x1.x2.g.y)

val point = #Point(1000, 2000)
assertResult(1000)(point.x)
assertResult(2000)(point.y)

val person = #Person("Kota Mizushima", 33, #Profile("Software Engineer", "Programming, Running, and Video Games"))
assertResult("Kota Mizushima")(person.name)
assertResult(33)(person.age)
assertResult("Software Engineer")(person.profile.description)
assertResult("Programming, Running, and Video Games")(person.profile.hobby)

val pair1: #Pair<Int, Int> = #Pair(1, 2)
assertResult(1)(pair1._1)
assertResult(2)(pair1._2)

val pair2 = #Pair(1.5, 2.5)
assertResult(1.5)(pair2._1)
assertResult(2.5)(pair2._2)

val pair3 = #Pair(true, 1)
assertResult(true)(pair3._1)
assertResult(1)(pair3._2)
