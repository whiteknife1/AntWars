package players.DarkPassengers

import scant._
import awannotation.AntWarsPlayer

@AntWarsPlayer
class DarkPassengersQueen extends Queen {
  def firstPlay = {
    if (countAnts < 2) layEgg(new Mesuka(400, 20, 0, 200))
    else if(countAnts<3) layEgg(new Lila(34, 4, false, 0))
    if (countAnts < 5){
      if (countAnts%3 == 1) layEgg(new Rita(400, 20, -10, 240))
      else if (countAnts%3 == 0) layEgg(new Rita(400, 20, 10, 240))
    }
    else if(countAnts < 7){
      if(countAnts%2 == 1) layEgg(new Lila(22, 10, false, 0))
      else if(countAnts%2 == 0) layEgg(new Lila(22, 10, false, 0))
    }
    else if(countAnts < 9){
      layEgg(new Mesuka(600, 10, 0, 250))
    }
}

  def takeTurn = {
    if(time<220) firstPlay
  }
}
//6: Enemy East
//7: Enemy North
//8: Enemy South
//9: Enemy West
abstract class Family extends Ant{
  var previous: Direction = North
  def basicTurn(y: Int) = {
    if(food > 0) moveTarget(0, 0)
    else if(lookForFood(Here)>0) pickup
    else if(findFood != Here){
      val dir: Direction = findFood
      if(canMove(dir)){
        move(dir)
      }
    }
    else{
      if(pos.x == 0) upDown = !upDown
      else if(pos.x > 22) upDown = !upDown
      previous = squiggle(y, previous)
      move(previous)
    }
  }
  def moveTarget(x: Int, y: Int) = {
     if(food > 0){
       if(Math.abs(pos.x)-Math.abs(x) <= 1){
         if(Math.abs(pos.y)-Math.abs(y) <= 1) drop
       }
     }
     if(pos.x > x){
        if(canMove(West)) move(West)
        else if(pos.y > y && canMove(North)) move(North)
        else if(canMove(South)) move(South)
        else move(East)
      }
      else if(pos.y > y){
        if(canMove(South)) move(South)
        else if(canMove(East)) move(East)
        else if(canMove(North)) move(North)
        else move(West)
      }
      else if(pos.y < y){
        if(canMove(North)) move(North)
        else if(canMove(East)) move(East)
        else if(canMove(South)) move(South)
        else move(West)
      }
      else if(pos.x <= x){
        if(canMove(East)) move(East)
        else if(canMove(South)) move(South)
        else if(canMove(North)) move(North)
        else move(West)
      }
  }

    def findFood: Direction = {
      if(lookForFood(West)>0){
          West
      }
      else if(lookForFood(East)>0){
          East
      }
      else if(lookForFood(North)>0){
          North
      }
      else if(lookForFood(South)>0){
          South
      }
      else{
        Here
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
}

class Rita(cap: Int, x: Int, y: Int, expTime: Int) extends Family { //Get food from center
    icon = 'R'
    capacity = cap //make function to generate optimal capacity given
    //# of ants
    strength = 1
    health = 3
    var moveCount = 0
    var doSquiggle = false
    def takeTurn = {
      //avoidEnemies
      if(time > expTime || doSquiggle) basicTurn(y)
      else if(food>0){
        moveTarget(10, 0)
        if(food == 0) moveCount = 0
      }
      else if(lookForFood(Here)>0 && pos.x > 13 && moveCount > 4){
        pickup
      }
      else if(findFood != Here && pos.x > 13 && moveCount > 4){
        val dir: Direction = findFood
        if(canMove(dir)){
          move(dir)
        }
      }
      else{
        moveTarget(x, y)
        moveCount += 1
      }
    }
}

class Mesuka(cap: Int, x: Int, y: Int, expTime: Int) extends Family { //Get food from center
    icon = 'M'
    capacity = cap //make function to generate optimal capacity given
    strength = 1
    health = 8
    var doSquiggle = false
    def takeTurn = {
      //avoidEnemies
      if(time > expTime || doSquiggle) basicTurn(y)
      else if(food>0){
        moveTarget(0, 0)
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
      else{
        moveTarget(x, y)
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
