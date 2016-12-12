package model

/**
  * Created by k.neyman on 10.12.2016.
  */
object Utils {
  def merge(old: Iterable[TaskWithTimeBound], updated: Iterable[TaskWithTimeBound]) = {
    old.filterNot(task => updated.exists(_.id == task.id)) ++ updated
  }
}
