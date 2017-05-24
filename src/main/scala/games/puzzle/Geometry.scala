package games.puzzle.geometry

import org.scalajs.dom
import scala.scalajs.js
import scala.util.Random

trait Drawable {
  type Ctx2D = dom.CanvasRenderingContext2D
  def at(p: Position): Ctx2D => Unit
}

trait Tile extends Drawable {
  def atOrigin(p: Position): Ctx2D => Unit
  def at(p: Position): Ctx2D => Unit
  //def at(p: Position)(context: Ctx2D) = Unit    // this is not the same!!
  val original: Tile
  val puzzle: Puzzle
  val isEmpty: Boolean
  val origin: Position
  var current: Position
}

class Image extends dom.html.Image {
  var onload: js.Function1[dom.Event, _] = ???
}

case class Square(size: Int) extends Drawable  {
  def at(p: Position) = (context: Ctx2D) => {
    context.beginPath()
    context.moveTo(p.x,p.y)
    context.lineTo(p.x+size,p.y)
    context.lineTo(p.x+size,p.y+size)
    context.lineTo(p.x,p.y+size)
    context.lineTo(p.x,p.y)
    context.stroke()
  }
}

case class Position(x: Int, y: Int) extends Ordered[Position] {
  def *(scale: Int): Position = Position(x*scale,y*scale)
  def +(p: Position): Position = Position(p.x+x,p.y+y)
  def mkString = "Position(" + x + "," + y + ")"
  def compare(that: Position): Int = {
    if(that.x == this.x) this.y compare that.y
    else this.x compare that.x
  }
  def above: Position = Position(x,y-1)
  def below: Position = Position(x,y+1)
  def left: Position = Position(x-1,y)
  def right: Position = Position(x+1,y)
}

case class Puzzle(image: Image, tileSize: Int, puzzleSize: Int, emptyTilePos: Position) extends Drawable {
  lazy val emptyTile: EmptyTile = EmptyTile(emptyTilePos,this)
  val tiles: Seq[Tile] = { for (x <- 0 to puzzleSize-1; y <- 0 to puzzleSize-1; pos = Position(x,y)) yield
    if(pos == emptyTilePos) emptyTile else NonEmptyTile(pos,this)
  }
  def shuffle: Unit = {
    val positions = for (x <- 0 to puzzleSize - 1; y <- 0 to puzzleSize - 1) yield Position(x, y)
    for ((tile, random) <- tiles.zip(getSolvableSeq)) tile.current = positions(random)
  }
  def at(p: Position) = (context: Ctx2D) => tiles foreach { t: Drawable => (t at p)(context) }
  private def getSolvableSeq: Seq[Int] = {    // get random sequence with even number of inversions
    def inversions(range: Seq[Int]): Int = {
      for (
        (i, index) <- range filter(_!=0) zipWithIndex;
        j <- range drop index+1 filter(_!=0); if(i>j)
      ) yield 1
    } length
    def isSolvable(range: Seq[Int]) = inversions(range) % 2 != 0
    val randomSeq: Seq[Int] = Random.shuffle(0 to 8).toIndexedSeq
    if (isSolvable(randomSeq)) { println("Order: " + randomSeq); randomSeq } else getSolvableSeq
  }
  def original = Puzzle(image, tileSize, puzzleSize, emptyTilePos)
  def tileAt(p: Position): Option[Tile] = tiles.find(t => t.current == p)
  def isSolved: Boolean = tiles.forall(t => t.current == t.origin)
}

case class EmptyTile(origin: Position, puzzle: Puzzle) extends Tile {
  def at(p: Position) = (context: Ctx2D) => Square(puzzle.tileSize).at(p+current*puzzle.tileSize)(context)
  def atOrigin(p: Position) = (context: Ctx2D) => { EmptyTile(origin, puzzle).at(p)(context) }
  val isEmpty = true
  var current = origin
  val original = if(current == origin) this else EmptyTile(origin, puzzle)
  def swapWith(that: Tile): Unit = {
    val thatCurrent = that.current
    that.current = current
    current = thatCurrent
  }
  def moveTo(p: Position): Unit = puzzle.tileAt(p) match {
    case Some(tile) => swapWith(tile)
    case None => println("Illegal Move. No Tile at "+p)
  }
  def moveUp: Unit = moveTo(current above)
  def moveDown: Unit = moveTo(current below)
  def moveLeft: Unit = moveTo(current left)
  def moveRight: Unit = moveTo(current right)
}

case class NonEmptyTile(origin: Position, puzzle: Puzzle) extends Tile {
  def at(p: Position) = (context: Ctx2D) => {
    //println("Drawing Tile from " + origin + " at " + current)
    context.drawImage(puzzle.image,
      origin.x * puzzle.tileSize, origin.y * puzzle.tileSize,
      puzzle.tileSize, puzzle.tileSize,
      current.x * puzzle.tileSize + p.x, current.y * puzzle.tileSize + p.y,
      puzzle.tileSize, puzzle.tileSize)
  }
  def atOrigin(p: Position) = (context: Ctx2D) => NonEmptyTile(origin, puzzle).at(p)(context)
  val isEmpty = false
  var current = origin
  val original = if(current == origin) this else NonEmptyTile(origin, puzzle)
}

case class Circle(radius: Int) extends Drawable {
  def at(p:Position) =
    (context: Ctx2D) => {
      context.beginPath()
      context.arc(p.x,p.y,radius,0,2*Math.PI)
      context.stroke()
    }
}
