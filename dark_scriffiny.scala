package players.DarkPassengers

import scant._
import awannotation.AntWarsPlayer

@AntWarsPlayer
class DarkPassengersQueen extends Queen {

  //hatch three Ritas for each hub
  //hatch Lila's to defend center hub
  //

  def takeTurn = {
    if (countAnts < 10){
      if (countAnts%2 == 1) layEgg(new Rita(false,400))
      //else if (countAnts%2 == 0 || countAnts%2 == 3) layEgg(new Lila(23, 2, false, 1))
      //else if (countAnts%2 == 0 || countAnts%2 == 3) layEgg(new Lila(4, 3, false 1))
      else if (countAnts%2 == 0 || countAnts%2 == 3) layEgg(new Doakes(2))
    }
    if (countAnts < 20){
      if (countAnts%1 == 1) layEgg(new Rita(true, 300))
      //else if (countAnts%2 == 0) layEgg(new Lila(23, 2, true, 1))
      //else if (countAnts%2 == 0) layEgg(new Lila(4, 3, true, 1))
      else if (countAnts%2 == 0) layEgg(new Doakes(0))
    }
  }

}

abstract class SerialKiller extends Ant {
  def findEnemy: Direction = {
    if(lookForEnemy(West)) West
    else if(lookForEnemy(East)) East
    else if(lookForEnemy(North)) North
    else if(lookForEnemy(South)) South
    else Here
  }

  def barricade(x_limit: Int, y_limit: Int) = {
      if (pos.y > -y_limit && pos.x < x_limit){
        if(canMove(South)) move(South)
        else if(canMove(East)) move(East)
        else if(canMove(North)) move(North)
        else move(West)
      }
      else if (pos.x > x_limit-1) {
        if (pos.y < y_limit) {
          if (canMove(North)) move(North)
          else if(canMove(West)) move(West)
          else move(South)
        }
        else move(West)
      }
      else move(East)
    }

  def huntQueen(attackStyle: Int) = {
    //attack from below
    if (attackStyle == 1 && pos.y > -12 && pos.x < 40) {
      if (canMove(South)) move(South)
      else move(East)
    }
    //attack from above
    if (attackStyle == 2 && pos.y < 12 && pos.x < 40) {
      if (canMove(North)) move(North)
      else move(East)
    }
    //attack from current position
    else {
      if (pos.x < 40) {
        if (canMove(East)) move(East)
        else if (canMove(South)) move(South)
        else if (canMove(North)) move(North)
        else move(East)
      }
      if (pos.y < 0) {
        if (canMove(North)) move(North)
        else if (canMove(South)) move(South)
      }
      if (pos.y > 0) {
        if (canMove(South)) move(South)
        else if (canMove(North)) move(North)
      }
    }
  }
}

//Defend with moving-blockade, then attack queen
class Lila(x_limit: Int, y_limit: Int, hunting: Boolean, attackStyle: Int) extends SerialKiller {
  icon = 'L'
  strength = 5
  health = 10

  def takeTurn = {
    if (findEnemy != Here) attack(findEnemy)
    if (hunting) huntQueen(attackStyle)
    else barricade(x_limit, y_limit)
  }
}

//Immediately Hunt Queen
class Doakes(attackStyle: Int) extends SerialKiller {
  icon = 'D'
  strength = 5 //25
  health = 5 //10

  def takeTurn = {
    if (findEnemy != Here) attack(findEnemy)
    else huntQueen(attackStyle)
  }
}


abstract class Family extends Ant{
  def returnHome() = {
      if(pos.x > 1){
        if(canMove(West)) move(West)
        else if(pos.y > 1 && canMove(North)) move(North)
        else if(canMove(South)) move(South)
        else move(East)
      }
      else if(pos.y > 1){
        if(canMove(South)) move(South)
        else if(canMove(East)) move(East)
        else if(canMove(North)) move(North)
        else move(West)
      }
      else if(pos.y < 1){
        if(canMove(North)) move(North)
        else if(canMove(East)) move(East)
        else if(canMove(South)) move(South)
        else move(West)
      }
      else if(pos.x <= 1) drop
    }

    def findFood: Direction = {
      if(lookForFood(West)>0) West
      else if(lookForFood(East)>0) East
      else if(lookForFood(North)>0) North
      else if(lookForFood(South)>0) South
      else Here
    }
    def goDir(left: Boolean): Unit = {
      if(left){
        if(canMove(North)) move(North)
        else move(East)
      }
      else {
        if(canMove(South)) move(South)
        else move(East)
      }
    }

    var upDown: Boolean = true
    var hold: Int = 0
    def squiggle(center: Int, prev: Direction): Direction = {
      if(hold > 0){
        hold -= 1
        return prev
      }
      else{
        hold = 2
      }
      val choose = random(4)
      if( Math.abs(pos.y)-Math.abs(center) > 5){
        if(pos.y > center){
          return South
        }
        else{
          return North
        }
      }
      if(choose == 1){
        return South
      }
      else if(choose == 2){
        return North
      }
      else{
        if(upDown){
          return West
        }
        else return East
      }
    }

    def avoidEnemies = {
      if(lookForEnemy(East)){
        leaveMessage(6)
        if(canMove(East)) move(East)
        else if(canMove(West)) move(West)
        else move(South)
      }
      else if(lookForEnemy(North)){
        leaveMessage(7)
        if(canMove(South)) move(South)
        else if(canMove(West)) move(West)
        else move(East)
      }
      else if(lookForEnemy(South)){
        leaveMessage(8)
        if(canMove(North)) move(North)
        else if(canMove(West)) move(West)
        else move(East)
      }
      else if(lookForEnemy(West)){
        leaveMessage(9)
        if(canMove(West)) move(West)
        else if(canMove(South)) move(South)
        else move(North)
      }
    }
}

class Rita(do_squiggle: Boolean, cap: Int) extends Family { //Get food from center
    icon = 'R'
    capacity = cap //make function to generate optimal capacity given
    //# of ants
    strength = 1
    health = 3
    var prev: Direction = North
    def takeTurn = {
      //avoidEnemies
      if(food>0){
        returnHome
      }
      else if(lookForFood(Here)>0){
        pickup
      }
      else if(findFood != Here){
        val dir: Direction = findFood
        if(canMove(dir)){
          move(dir)
        }
      }
      else if(do_squiggle){
        if(pos.x == 0) upDown = !upDown
        else if(pos.x > 22) upDown = !upDown
        prev = squiggle(21, prev)
        move(prev)
      }
      else{
        if(canMove(East)) move(East)
        else if(canMove(North)) move(North)
        else move(South)
      }
    }
}
class Cody(do_squiggle:Boolean, cap: Int) extends Family { //Get food from Right
    icon = 'C'
    capacity = cap //make function to generate optimal capacity given
    //# of ants
    strength = 1
    health = 3
    var prev: Direction = North

    def takeTurn = {
      //avoidEnemies
      if(food > 0) returnHome
      else if(lookForFood(Here)>0){
        pickup
      }
      else if(findFood != Here){
        move(findFood)
      }
      else if(pos.y < 10){
        goDir(true)
      }
      else if(do_squiggle){
        if(pos.x == 0) upDown = !upDown
        else if(pos.x > 22) upDown = !upDown
        prev = squiggle(10, prev)
        move(prev)
      }
      else{
        if(canMove(East)) move(East)
        else if (canMove(North)) move(North)
        else if(canMove(West)) move(West)
        else move(South)
      }
    }
}

class Aster(do_squiggle: Boolean, cap: Int) extends Family { //Get food Left
    icon = 'A'
    capacity = cap //make function to generate optimal capacity given
    //# of ants
    strength = 1
    health = 3
    var prev: Direction = North

    def takeTurn = {
      //avoidEnemies
      if(food > 0) returnHome
      else if(lookForFood(Here)>0){
        pickup
      }
      else if(findFood != Here){
        move(findFood)
      }
      else if(pos.y > -10){
        goDir(false)
      }
      else if(do_squiggle){
        if(pos.x == 0) upDown = !upDown
        else if(pos.x > 22) upDown = !upDown
        prev = squiggle(-10, prev)
        move(prev)
      }
      else{
        if(canMove(East)) move(East)
        else if (canMove(South)) move(South)
        else if(canMove(West)) move(West)
        else move(North)
      }
    }
}
