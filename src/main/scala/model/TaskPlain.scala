package model

import model.TaskType.TaskType

/**
  * Created by k.neyman on 09.12.2016.
  */

class TaskPlain(val id: Int,
                val length: Int,
                val taskType: TaskType = TaskType.NoMatter) {
  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case t: TaskPlain => t.id == id
      case _ => false
    }
  }
}

object TaskPlain {
  def apply(id: Int, length: Int, taskType: TaskType = TaskType.NoMatter): TaskPlain =
    new TaskPlain(id, length, taskType)
}

