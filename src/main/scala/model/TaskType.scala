package model

/**
  * Created by k.neyman on 10.12.2016.
  */
object TaskType extends Enumeration {
  type TaskType = Value
  val ASAP, ALAP, NoMatter = Value
}
