package model

import scala.language.postfixOps

/**
  * Created by k.neyman on 09.12.2016.
  */
trait TaskSource {
  val tasks: List[TaskPlain]
  lazy val tasksWithTimeBound: List[TaskWithTimeBound] = tasks map (new TaskWithTimeBound(_))
  val links: List[Link]

  lazy val last = {
    val beforeIds = links map(_.idBefore)
    tasks map(_.id) filterNot(beforeIds contains _)
  }

  lazy val root = {
    val afterIds = links map(_.idAfter)
    tasks map(_.id) filterNot(afterIds contains _)
  }

  def findRoutes(taskId: Int): List[Int] = {
    taskId :: findParents(taskId) ::: findChilds(taskId)
  }

  def findParents(taskId: Int): List[Int] = {
    val before = this.before(taskId)
    before ::: (before flatMap findParents)
  }

  def findChilds(taskId: Int): List[Int] = {
    val next = after(taskId)
    next ::: (next flatMap findChilds)
  }

  def before(taskId: Int) = links filter(_.idAfter == taskId) map(_.idBefore)
  def after(taskId: Int) = links filter(_.idBefore == taskId) map(_.idAfter)

  def sortTasks(implicit idToTask: Map[Int, Task]): List[Task] = {
    val last = this.last sortBy sortFun
    last.foldLeft(last) { sortTasks } map idToTask
  }

  def sortTasks(ids: List[Int], id: Int)(implicit idToTask: Map[Int, Task]) : List[Int] = {
    val (beforeId, afterId) = ids.span(_ != id)
    val parents = before(id) sortBy sortFun filterNot(beforeId contains _)
    parents.foldLeft(beforeId ::: parents ::: afterId) { sortTasks }
  }

  def sortFun(id: Int)(implicit idToTask: Map[Int, Task]) = {
    val t = idToTask(id)
    (links.count(_.idAfter == id), t.endMax, -t.length)
  }
}
