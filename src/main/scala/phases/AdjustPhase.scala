package phases

import model.{Link, Task, TaskType, TaskWithTimeBound}

import scala.language.postfixOps

/**
  * Created by k.neyman on 10.12.2016.
  */
class AdjustPhase(val tasks: List[TaskWithTimeBound], val links: List[Link]) {
  def execute(): List[Task] = {
    val (asaps, others) = tasks partition(_.taskType == TaskType.ASAP)
    val asapTasks = asaps map { task => Task(task.startMin, task.endMin, task)}
    val (updated, left) = checkBefore(asapTasks, others)

    val (alaps, noMatters) = left partition { _.taskType == TaskType.ALAP }
    val alapTasks = alaps map { task => Task(task.startMax, task.endMax, task) }
    val (updated2, left2) = checkAfter(alapTasks, noMatters)

    val noMattersTasks = left2 map { task => Task(task.startMin, task.endMin, task) }

    (asapTasks ::: updated ::: alapTasks ::: updated2 ::: noMattersTasks).sortBy(_.id)
  }

  def checkBefore(tasksUpdated: List[Task],
            others: List[TaskWithTimeBound]): (List[Task], List[TaskWithTimeBound]) = {
    if (tasksUpdated.isEmpty)
      (tasksUpdated, others)
    else {
      val partDefined = tasksUpdated flatMap { task =>
        links filter
          {  _.idAfter == task.id } flatMap
          { id => others.find(_.id == id.idBefore) } flatMap
          { t =>
            if (t.endMin.get == task.startMin.get) Some(Task(t.startMin, t.endMin, t))
            else None
          }
      } groupBy (_.id) map { case (_, task) => task.head } toList
      val (updated, othersOnceMore) = checkBefore(
        partDefined,
        others filterNot (task => partDefined.exists(_.id == task.id))
      )
      (partDefined ::: updated, othersOnceMore)
    }
  }

  def checkAfter(tasksUpdated: List[Task],
                  others: List[TaskWithTimeBound]): (List[Task], List[TaskWithTimeBound]) = {
    if (tasksUpdated.isEmpty)
      (tasksUpdated, others)
    else {
      val partDefined = tasksUpdated flatMap { task =>
        links filter
          { _.idBefore == task.id } flatMap
          { id => others.find(_.id == id.idAfter) } flatMap
          { t =>
            if (t.startMax.get == task.endMax.get) Some(Task(t.startMax, t.endMax, t))
            else None
          }
      } groupBy (_.id) map { case (_, task) => task.head } toList
      val (updated, othersOnceMore) = checkAfter(
        partDefined,
        others filterNot (task => partDefined.exists(_.id == task.id))
      )
      (partDefined ::: updated, othersOnceMore)
    }
  }
}
