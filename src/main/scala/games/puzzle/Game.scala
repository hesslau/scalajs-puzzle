package games.puzzle

import games.puzzle.geometry._
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}
import scala.scalajs.js.annotation.JSExport

object Game extends js.JSApp {
  type Ctx2D = dom.CanvasRenderingContext2D

  val canvas = {
    createCanvasElement
  }
  val ctx = get2DContext(canvas)

  def draw(f: dom.CanvasRenderingContext2D => Unit) = f(ctx)
  def drawIt(d: Drawable) = (d at Position(0,0))(ctx)
  val image = createImageElement
  val puzzle = {
    image.src = "image.png"
    Puzzle(image,100,3,Position(2,0))
  }

  def moveDown = puzzle.emptyTile.moveUp
  def moveRight = puzzle.emptyTile.moveLeft
  def moveUp = puzzle.emptyTile.moveDown
  def moveLeft = puzzle.emptyTile.moveRight
  def reDraw: Unit = {
    ctx.clearRect ( 0 , 0 , canvas.width, canvas.height )
    drawIt(puzzle)
    if(puzzle.isSolved) println("Puzzle solved!")
  }

  dom.document.onkeydown = (e: dom.KeyboardEvent) => e.keyCode match {
      case 65 | 37 => moveLeft; reDraw
      case 68 | 39 => moveRight; reDraw
      case 87 | 38 => moveUp; reDraw
      case 83 | 40 => moveDown; reDraw
      case 81 => puzzle.shuffle; reDraw
      case x => log("Key " + x + " not found.")
  }


  @JSExport
  def startGame: Unit = {
    log("Starting Game")
    canvas.width = 300
    canvas.height = 300
    addToBody(canvas)
    puzzle.shuffle
    reDraw
  }

  @JSExport
  def main(): Unit = image.onload = (e: dom.Event) => startGame

  private def createImageElement: Image = dom.document.createElement("img").asInstanceOf[Image]
  private def createCanvasElement: html.Canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
  private def get2DContext(c: html.Canvas): Ctx2D = c.getContext("2d").asInstanceOf[Ctx2D]
  private def addToBody(element: dom.raw.HTMLElement): Unit = dom.document.body.appendChild(element)
  private def log[A](x: A): A = {
    println(x); x
  }
}