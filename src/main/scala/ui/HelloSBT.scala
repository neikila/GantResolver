package ui

import phases.Main
import model.{Task, TaskSourceImpl, TaskType}

import scala.collection.immutable.HashMap
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.postfixOps
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.{Node, Scene}
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{Color, LinearGradient, Stops}
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.text.Text

object HelloSBT extends JFXApp {
  val source = new TaskSourceImpl
  val links = source.links
  val tasks = Await.result(Main.calculate(source), Duration.Inf)
  val taskIdToMap = scala.collection.mutable.HashMap.empty[Int, Rectangle]
  implicit val idToTask: Map[Int, Task] = tasks.map(task => task.id -> task) toMap

  val coveredColor = Green
  val defaultColor = Orange
  val criticalColor = Red

  val taskHeight = 16
  val offsetY = 8
  val offsetX = 5

  val startX = 25
  val startFrom = 40
  val delta = taskHeight + 2 * offsetY + 2 * 3

  val dayWidth = 30
  val zeroDayPos = 100

  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Stage"
    width = 1200
    height = 600
    scene = new Scene {
      fill = LightGray
      content = drawField ::: (source.sortTasks.zipWithIndex flatMap { case (task, i) => convert(task, i) })
    }
  }

  def defaultTaskColor(task: Task) = {
    if (task.isCritical) criticalColor
    else defaultColor
  }

  def select(task: Task): Unit = {
    source.findRoutes(task.id).map(taskIdToMap).foreach(_.fill = Blue)
  }

  def unselect(task: Task)(implicit idToTask: Map[Int, Task]): Unit = {
    source.findRoutes(task.id)
      .flatMap(el => idToTask get el map { task => (el, task) } )
      .foreach { case (id, taskLinked) => taskIdToMap(id).fill = defaultTaskColor(taskLinked) }
  }

  def drawField: List[Node] = {
    val from = this.startX + zeroDayPos
    val lines = (0 to tasks.maxBy(_.endMax.get).endMax.get) map { i =>
      new Line {
        startX = (from + i * dayWidth).toDouble
        endX = (from + i * dayWidth).toDouble
        startY = startFrom.toDouble
        endY = (startY + tasks.length * delta).toDouble
        stroke = Color(100.0 / 256, 100.0 / 256, 100.0 / 256, 0.7)
      }
    } toList

    val header = (0 until tasks.maxBy(_.endMax.get).endMax.get) map { i =>
      new Text {
        x = (from + i * dayWidth).toDouble + 4
        y = startFrom.toDouble - 2
        text = s"${i + 1}"
        style = "-fx-font-size: 13pt"
        fill = Black
      }
    }
    lines ::: header.toList
  }

  def convert(task: Task, pos: Int): List[Node] = {
    val startY = startFrom + delta * pos

    val rect = new Rectangle {
      x = startX + zeroDayPos + task.start.get * dayWidth
      y = startY
      width = task.length * dayWidth
      height = taskHeight + 2 * offsetY
      fill = defaultTaskColor(task)
      onMouseEntered = () => { select(task) }
      onMouseExited = () => { unselect(task) }
    }
    val total = new Rectangle {
      x = startX + zeroDayPos + task.startMin.get * dayWidth
      y = startY
      width = dayWidth * (task.endMax.get - task.startMin.get)
      height = taskHeight + 2 * offsetY
      fill = Gray
    }

    taskIdToMap put (task.id, rect)

    List (
      total,
      rect,
      new Text {
        x = startX
        y = startY + taskHeight + offsetY
        text = s"Task #${task.id}"
        style = "-fx-font-size: 16pt"
        fill = Black
      },
      new Text {
        x = startX + zeroDayPos + task.startMin.get * dayWidth + 2
        y = startY + taskHeight + offsetY
        text = task.taskType match {
          case TaskType.ALAP => "ALAP"
          case TaskType.ASAP => "ASAP"
          case _ => ""
        }
        style = "-fx-font-size: 7pt"
        fill = { val c = Black; c.opacity(0.8); c }
      }
    )
  }
}
