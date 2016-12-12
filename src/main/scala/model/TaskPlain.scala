package model

import model.TaskType.TaskType

/**
  * Created by k.neyman on 09.12.2016.
  */

class TaskPlain(val id: Int,
                val length: Int,
                val taskType: TaskType = TaskType.NoMatter)

object TaskPlain {
  def apply(id: Int, length: Int, taskType: TaskType = TaskType.NoMatter): TaskPlain =
    new TaskPlain(id, length, taskType)
}

