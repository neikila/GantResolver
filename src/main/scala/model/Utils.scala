package model

/**
  * Created by k.neyman on 10.12.2016.
  */
object Utils {
  def merge(old: Seq[TaskWithTimeBound], updated: Seq[TaskWithTimeBound]) = {
    old.filterNot(updated contains _) ++ updated
  }
}
