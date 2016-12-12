package model

/**
  * Created by k.neyman on 10.12.2016.
  */
class TaskWithTimeBound(task: TaskPlain,
                        val startMin: Option[Int],
                        val endMax: Option[Int]) extends TaskPlain(task.id, task.length, task.taskType) {
  def this(task: TaskPlain) { this(task, None, None) }

  def endMin: Option[Int] = startMin map { _ + length }
  def startMax: Option[Int] = endMax map { _ - length }

  def isCritical = endMax.get - startMin.get == length

  def updateStartMin(time: Int) = TaskWithTimeBound(this, Some(time), endMax)
  def updateStartMin(time: Option[Int]) = TaskWithTimeBound(this, time, endMax)

  def updateEndMax(time: Int) = TaskWithTimeBound(this, startMin, Some(time))
  def updateEndMax(time: Option[Int]) = TaskWithTimeBound(this, startMin, time)

  override def toString: String = {
    s"Task #$id: Min($startMin, $endMin), Max($startMax, $endMax)"
  }
}

object TaskWithTimeBound {
  def apply(task: TaskPlain, startMin: Option[Int], endMax: Option[Int]): TaskWithTimeBound =
    new TaskWithTimeBound(task, startMin, endMax)

  def apply(task: TaskPlain): TaskWithTimeBound =
    new TaskWithTimeBound(task)
}
