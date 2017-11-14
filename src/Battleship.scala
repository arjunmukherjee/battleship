object Battleship extends App {
  Battleship(Player("Ann"), Player("John")).start()
}
object Coordinate {
  private def inputIsValid(coordinate: String) : Boolean = {
    coordinate.matches("([0-9]|1[0-9]|2[0-9]),([0-9]|1[0-9]|2[0-9])")
  }
  def constructCoordinate(input: String) : Option[Coordinate] = {
    if (inputIsValid(input)) {
      val inputArray = input.split(",")
      Some(Coordinate(Integer.parseInt(inputArray(0)), Integer.parseInt(inputArray(1))))
    } else {
      None
    }
  }
}
case class Coordinate(x: Int, y: Int)
case class Ship(name: String, positions: Set[Coordinate])

case class Player(name: String) {
  private val GOT_HIT = true
  private val fleet = addShipsToBoard()
  private val allCoordinatesWithState = (fleet flatMap { _.positions }).foldLeft(collection.mutable.Map.empty[Coordinate, Boolean]){(accumulator: collection.mutable.Map[Coordinate, Boolean],coordinate: Coordinate) => accumulator + (coordinate -> false) }

  def allShipsSunk: Boolean = {
    allCoordinatesWithState.values.count(!_) <= 0
  }

  private def addShipsToBoard() = {
    val fleet = Set(Ship("Explorer",Set(Coordinate(1,1), Coordinate(1,2), Coordinate(1,3))),
                    Ship("Battleship",Set(Coordinate(2,1), Coordinate(2,2), Coordinate(2,3))),
                    Ship("Submarine",Set(Coordinate(3,2), Coordinate(4,2), Coordinate(5,2), Coordinate(6,2))),
                    Ship("Carrier",Set(Coordinate(10,10), Coordinate(10,9), Coordinate(10,8), Coordinate(10,7),Coordinate(10,6))),
                    Ship("Cruiser",Set(Coordinate(5,3), Coordinate(6,3))))
    fleet.take(scala.util.Random.nextInt(fleet.size)+1)
  }

  def applyMove(coordinate: Coordinate) : Unit = {
      allCoordinatesWithState.get(coordinate) match {
      case Some(isHit) =>
        if (isHit) {
          println("Already taken")
        } else {
          println("That's a HIT!")
          allCoordinatesWithState.put(coordinate, GOT_HIT)
          fleet foreach { ship =>
            if (ship.positions.contains(coordinate)) {
              if (!ship.positions.exists{!allCoordinatesWithState.getOrElse(_,false)}) {
                println(s"${ship.name} SUNK!!")
                return
              }
            }
          }
        }
      case None => println("MISS!")
    }
  }
}

trait Game {
  def start(): Unit
  def end(message: String): Unit
}

case class Battleship(playerOne: Player, playerTwo: Player) extends Game {
  @Override
  def start(): Unit = {
    println(s"\n\tStarting a 30x30 game of BATTLESHIP between ${playerOne.name} and ${playerTwo.name}")
    println(s"\t(Your ships have been randomly placed on the board)")
    while(true) {
      makeAMove(playerOne,playerTwo)
      makeAMove(playerTwo,playerOne)
    }
  }

  override def end(message: String): Unit = {
    println(message)
    System.exit(0)
  }

  private def makeAMove(playerMakingMove: Player, otherPlayer: Player) = {
    println(s"\n${playerMakingMove.name.toUpperCase}, pick a coordinate :")
    Coordinate.constructCoordinate(scala.io.StdIn.readLine()) match {
      case Some(coordinate) =>
        otherPlayer.applyMove(coordinate)
        if (otherPlayer.allShipsSunk) {
          end(s"${playerMakingMove.name} you WIN!!")
        }
      case _ => println(s"Incorrect input provided")
    }
  }
}
