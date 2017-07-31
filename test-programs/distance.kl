record Player {
  x: Int; y: Int
  name: String
  hp: Int
  wp: Int
}

record Enemy {
  x: Int; y: Int
  name: String
  hp: Int
}

def distance(p, e) = {
  val dx = abs(double(p.x - e.x))
  val dy = abs(double(p.y - e.y))
  sqrt(dx * dx + dy * dy)
}

val p = #Player(4, 6, "Hero", 50, 10)
val e = #Enemy(1, 2, "Slime", 5)
assertResult(5.0)(distance(p, e))