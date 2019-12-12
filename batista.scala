//Center-hub: 21, 3, 0
//Top-Hub: 21, 10, 10
//Bottom-Hub: 21, 10, -10

//Stationary Defense
class Batista(x_limit: Int, y_limit: Int, y_lock: Int) extends SerialKiller {
  icon = 'B'
  strength = 10
  health = 11

  var parked = false
  def takeTurn = {
    if (findEnemy != Here) attack(findEnemy)
    else {
      if (pos.x == x_limit && pos.y == y_lock) parked = true
      if (parked) move(Here)
      else barricade(x_limit, y_limit)
    }
  }
}
