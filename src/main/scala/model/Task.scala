package model

/**
  * Created by k.neyman on 10.12.2016.
  */
class Task(val start: Option[Int], val end: Option[Int], timeBounds: TaskWithTimeBound) extends
  TaskWithTimeBound(timeBounds, timeBounds.startMin, timeBounds.endMax) {
  override def toString: String = s"Task #$id. Time: ($start, $end)"
}

object Task {
  def apply(start: Option[Int], end: Option[Int], taskWithTimeBound: TaskWithTimeBound) =
    new Task(start, end, taskWithTimeBound)

  def apply(taskWithTimeBound: TaskWithTimeBound) =
    new Task(None, None, taskWithTimeBound)
}
