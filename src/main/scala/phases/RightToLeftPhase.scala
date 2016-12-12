package phases

import model.{Link, TaskSource, TaskWithTimeBound, Utils}

import scala.language.postfixOps

/**
  * Created by k.neyman on 10.12.2016.
  */
class RightToLeftPhase {
  private def endTasks(source: TaskSource) = {
    val idsBefore = source.links.map(_.idBefore)
    source.tasksWithTimeBound.filterNot(task => idsBefore.contains(task.task.id))
  }

  def rightToLeft(source: TaskSource, deadline: Int): Iterable[TaskWithTimeBound] = {
    val tasks = endTasks(source) map { _.updateEndMax(deadline) }
    val all = Utils.merge(source.tasksWithTimeBound, tasks)
    rightToLeft(tasks, all, source.links)
  }

  private def rightToLeft(init: Iterable[TaskWithTimeBound],
                  all: Iterable[TaskWithTimeBound],
                  links: Iterable[Link]): Iterable[TaskWithTimeBound] = {
    val updated = init flatMap
      { beforeThat(all, links)(_) } groupBy
      { _.task.id } map
      { case(id, tasks) => tasks.minBy(_.endMax.get) }
    if (updated isEmpty) all
    else rightToLeft(updated, Utils.merge(all, updated), links)
  }

  private def beforeThat(allTasks: Iterable[TaskWithTimeBound], links: Iterable[Link])(task: TaskWithTimeBound): Iterable[TaskWithTimeBound] = {
    links filter { _.idAfter == task.task.id } map
      { link => allTasks.find(_.task.id == link.idBefore).get } filter
      { _.endMax.forall(_ > task.startMax.get) } map
      { _.updateEndMax(task.startMax) }
  }
}
