package phases

import model.{Link, TaskSource, TaskWithTimeBound, Utils}

import scala.language.postfixOps

/**
  * Created by k.neyman on 10.12.2016.
  */
class LeftToRightPhase {
  def startTasks(implicit source: TaskSource) = {
    val idsAfter = source.links.map(_.idAfter)
    source.tasksWithTimeBound.filterNot(task => idsAfter.contains(task.id))
  }

  def leftToRight(source: TaskSource): Iterable[TaskWithTimeBound] = {
    val tasks = startTasks(source) map { _.updateStartMin(0) }
    val all = Utils.merge(source.tasksWithTimeBound, tasks)
    leftToRight(tasks, all, source.links)
  }

  def leftToRight(init: Iterable[TaskWithTimeBound],
                  all: Iterable[TaskWithTimeBound],
                  links: Iterable[Link]): Iterable[TaskWithTimeBound] = {
    val updated = init flatMap
      { nextTo(all, links)(_) } groupBy
      { _.task.id } map
      { case(id, tasks) => tasks.maxBy(_.startMin.get) }
    if (updated isEmpty) all
    else leftToRight(updated, Utils.merge(all, updated), links)
  }

  def nextTo(allTasks: Iterable[TaskWithTimeBound], links: Iterable[Link])(task: TaskWithTimeBound): Iterable[TaskWithTimeBound] = {
    links filter { _.idBefore == task.task.id } map
      { link => allTasks.find(_.task.id == link.idAfter).get } filter
      { _.startMin.forall(_ < task.endMin.get) } map
      { _.updateStartMin(task.endMin) }
  }
}
