package model

/**
  * Created by k.neyman on 10.12.2016.
  */
class TaskWithTimeBound(val task: TaskPlain,
                        val startMin: Option[Int],
                        val endMax: Option[Int]) extends TaskPlain(task.id, task.length, task.taskType) {
  def this(task: TaskPlain) { this(task, None, None) }

  def endMin: Option[Int] = startMin map { _ + task.length }
  def startMax: Option[Int] = endMax map { _ - task.length }

  def isCritical = endMax.get - startMin.get == task.length

  def updateStartMin(time: Int) = TaskWithTimeBound(task, Some(time), endMax)
  def updateStartMin(time: Option[Int]) = TaskWithTimeBound(task, time, endMax)

  def updateEndMax(time: Int) = TaskWithTimeBound(task, startMin, Some(time))
  def updateEndMax(time: Option[Int]) = TaskWithTimeBound(task, startMin, time)

  override def toString: String = {
    s"Task #${task.id}: Min($startMin, $endMin), Max($startMax, $endMax)"
  }
}

object TaskWithTimeBound {
  def apply(task: TaskPlain, startMin: Option[Int], endMax: Option[Int]): TaskWithTimeBound =
    new TaskWithTimeBound(task, startMin, endMax)
}
