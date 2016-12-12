package phases

import model.{Link, TaskSource, TaskWithTimeBound, Utils}

import scala.language.postfixOps

/**
  * Created by k.neyman on 10.12.2016.
  */
class RightToLeftPhase(implicit val source: TaskSource) {
  private def endTasks() = {
    val idsBefore = source.links.map(_.idBefore).toSet
    source.tasksWithTimeBound filterNot(idsBefore contains _.id)
  }

  def rightToLeft(deadline: Int): Iterable[TaskWithTimeBound] = {
    val tasks = endTasks map { _ updateEndMax deadline }
    val all = Utils.merge(source.tasksWithTimeBound, tasks)
    rightToLeft(tasks, all, source.links)
  }

  private def rightToLeft(init: Seq[TaskWithTimeBound],
                          all: Seq[TaskWithTimeBound],
                          links: Seq[Link]): Seq[TaskWithTimeBound] = {
    val updated = init flatMap
      { beforeThat(all, links)(_) } groupBy
      { _.id } map
      { case(id, tasks) => tasks.minBy(_.endMax.get) } toSeq

    if (updated isEmpty) all
    else rightToLeft(updated, Utils.merge(all, updated), links)
  }

  private def beforeThat(allTasks: Iterable[TaskWithTimeBound], links: Iterable[Link])(task: TaskWithTimeBound): Iterable[TaskWithTimeBound] = {
    for {
      link <- links
      if link.idAfter == task.id
      taskBefore <- allTasks
      if taskBefore.id == link.idBefore
      if taskBefore.endMax.forall(_ > task.startMax.get)
    } yield taskBefore.updateEndMax(task.startMax)
  }
}
