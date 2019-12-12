package players.DarkPassengers

import scant._
import awannotation.AntWarsPlayer

@AntWarsPlayer
class DarkPassengersQueen extends Queen {
  def takeTurn = {
    if (countAnts < 5){
      if (countAnts%3 == 1) layEgg(new Mesuka(400, 20, 0))
      else if (countAnts%3 == 0) layEgg(new Rita(400, 20, 10))
      else layEgg(new Rita(400, 20, -10))
    }
    else if(countAnts < 7){
      layEgg(new Mesuka(600, 10, 0))
    }
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

class Rita(cap: Int, x: Int, y: Int) extends Family { //Get food from center
    icon = 'R'
    capacity = cap //make function to generate optimal capacity given
    //# of ants
    strength = 1
    health = 3
    var moveCount = 0
    var doSquiggle = false
    def takeTurn = {
      //avoidEnemies
      if(time > 220 || doSquiggle) basicTurn(y)
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

class Mesuka(cap: Int, x: Int, y: Int) extends Family { //Get food from center
    icon = 'M'
    capacity = cap //make function to generate optimal capacity given
    strength = 1
    health = 8
    var doSquiggle = false
    def takeTurn = {
      //avoidEnemies
      if(time > 250 || doSquiggle) basicTurn(y)
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
