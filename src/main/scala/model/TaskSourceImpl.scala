package model

/**
  * Created by k.neyman on 10.12.2016.
  */
class TaskSourceImpl extends TaskSource {
  val tasks =
    TaskPlain(0, 2) :: TaskPlain(1, 3) :: TaskPlain(2, 3, TaskType.ALAP) :: TaskPlain(3, 5) ::
    TaskPlain(4, 2, TaskType.ASAP) :: TaskPlain(5, 2) :: TaskPlain(8, 1, TaskType.ALAP) :: TaskPlain(7, 1) ::
    TaskPlain(6, 5) :: Nil

  val links = Link(0, 1) :: Link(1, 3) :: Link(3, 6) ::
    Link(4, 5) :: Link(5, 8) :: Link(8, 7) :: Link(7, 6) ::
    Link(1, 2) ::
    Nil
}
