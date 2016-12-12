package phases

import model.{Link, TaskSource, TaskWithTimeBound, Utils}

import scala.language.postfixOps

/**
  * Created by k.neyman on 10.12.2016.
  */
class LeftToRightPhase {
  def startTasks(implicit source: TaskSource) = {
    val idsAfter = source.links map(_.idAfter)
    source.tasksWithTimeBound filterNot(idsAfter contains _.id)
  }

  def leftToRight(source: TaskSource): Iterable[TaskWithTimeBound] = {
    val tasks = startTasks(source) map { _ updateStartMin 0 }
    val all = tasks.foldLeft(source.idToTask) { (map, task) => map + (task.id -> task) }
    leftToRight(tasks)(all, source.links)
  }

  def leftToRight(init: Seq[TaskWithTimeBound])(
                  implicit all: Map[Int, TaskWithTimeBound], links: Seq[Link]): Seq[TaskWithTimeBound] = {
    val updated = init flatMap { nextTo } groupBy { _.id } map
      { case(_, tasks) => tasks.maxBy(_.startMin.get) } toSeq

    if (updated isEmpty) all.values toSeq
    else leftToRight(updated)(updated.foldLeft(all) {(map, task) => map + (task.id -> task)}, links)
  }

  def nextTo(task: TaskWithTimeBound)(implicit allTasks: Map[Int, TaskWithTimeBound],  links: Iterable[Link]): Iterable[TaskWithTimeBound] = {
    for {
      link <- links
      if link.idBefore == task.id
      taskAfter <- allTasks get link.idAfter
      if taskAfter.startMin.forall(_ < task.endMin.get)
    } yield taskAfter.updateStartMin(task.endMin)
  }
}
