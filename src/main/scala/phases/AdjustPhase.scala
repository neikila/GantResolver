package phases

import model.{Link, Task, TaskType, TaskWithTimeBound}

import scala.language.{implicitConversions, postfixOps}

/**
  * Created by k.neyman on 10.12.2016.
  */
class AdjustPhase(val tasks: List[TaskWithTimeBound], val links: List[Link]) {
  implicit def convertWithBounds(list: List[TaskWithTimeBound]): Map[Int, TaskWithTimeBound] = { list map (t => t.id -> t) toMap }
  implicit def convert(list: List[Task]): Map[Int, Task] = { list map (t => t.id -> t) toMap }
  implicit def convertWithBounds(list: Map[Int, TaskWithTimeBound]): List[TaskWithTimeBound] = (list values).toList
  implicit def convert(list: Map[Int, Task]): List[Task] = (list values).toList

  def execute(): List[Task] = {
    val (asaps, others) = convertWithBounds(tasks) partition(_._2.taskType == TaskType.ASAP)
    val asapTasks = asaps map { case(id, task) => id -> Task(task.startMin, task.endMin, task) }
    val (updated, left) = checkBefore(asapTasks, others)

    val (alaps, noMatters) = left partition { _._2.taskType == TaskType.ALAP }
    val alapTasks = alaps map { case(id, task) => id -> Task(task.startMax, task.endMax, task) }
    val (updated2, left2) = checkAfter(alapTasks, noMatters)

    val noMattersTasks = left2 map { case(id, task) => id -> Task(task.startMin, task.endMin, task) }

    (asapTasks ++ updated ++ alapTasks ++ updated2 ++ noMattersTasks).sortBy(_.id)
  }

  def checkBefore(tasksUpdated: Map[Int, Task],
                  others: Map[Int, TaskWithTimeBound]): (Map[Int, Task], Map[Int, TaskWithTimeBound]) = {
    if (tasksUpdated.isEmpty)
      (tasksUpdated, others)
    else {
      val partDefined = (for {
        task <- tasksUpdated.values
        link <- links if link.idAfter == task.id
        taskOther <- others get link.idBefore
        if taskOther.endMin.get == task.startMin.get
      } yield Task(taskOther.startMin, taskOther.endMin, taskOther))
        .groupBy (_.id) map { case (id, task) => id -> task.head }
      val (updated, othersOnceMore) = checkBefore(
        partDefined,
        others filterKeys (!partDefined.contains(_))
      )
      (partDefined ++ updated, othersOnceMore)
    }
  }

  def checkAfter(tasksUpdated: Map[Int, Task],
                  others: Map[Int, TaskWithTimeBound]): (Map[Int, Task], Map[Int, TaskWithTimeBound]) = {
    if (tasksUpdated.isEmpty)
      (tasksUpdated, others)
    else {
      val partDefined =
        (for {
          task <- tasksUpdated.values
          link <- links if link.idBefore == task.id
          taskOther <- others get link.idAfter
          if taskOther.startMax.get == task.endMax.get
        } yield Task(taskOther.startMax, taskOther.endMax, taskOther))
          .groupBy (_.id) map { case (id, task) => id -> task.head }
      val (updated, othersOnceMore) = checkAfter(
        partDefined,
        others filterKeys (!partDefined.contains(_))
      )
      (partDefined ++ updated, othersOnceMore)
    }
  }
}
