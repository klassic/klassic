record P {
  x: Int
  y: Int
  z: Int
}
record Q {
  x: Double
  y: Double
  z: Double
}

record T <'a, 'b> {
  x: 'a
  y: 'b
}

def add_xy(o) = {
  o.x + o.y
}
assert(3 == add_xy(#P(1, 2, 3)))
assert(3 == add_xy(#T(1, 2)))
