package model

/**
  * Created by k.neyman on 10.12.2016.
  */
class Task(val start: Option[Int], val end: Option[Int], val timeBounds: TaskWithTimeBound) {
  def this(task: TaskWithTimeBound) {
    this(None, None, task)
  }

  override def toString: String = s"Task #$id. Time: ($start, $end)"

  def id = timeBounds.id
  def length = timeBounds.length

  def startMin = timeBounds.startMin
  def endMin = timeBounds.endMin

  def startMax = timeBounds.startMax
  def endMax = timeBounds.endMax

  def isCritical = timeBounds.isCritical

  def taskType = timeBounds.task.taskType
}

object Task {
  def apply(start: Option[Int], end: Option[Int], taskWithTimeBound: TaskWithTimeBound) =
    new Task(start, end, taskWithTimeBound)

  def apply(taskWithTimeBound: TaskWithTimeBound) =
    new Task(None, None, taskWithTimeBound)
}
