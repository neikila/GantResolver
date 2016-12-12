import model._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import phases._

import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class TaskSourceSuite extends FunSuite {
  class TaskSourceDebugV1Impl extends TaskSource {
    val tasks = TaskPlain(0, 2) :: TaskPlain(1, 3) :: TaskPlain(2, 5) :: Nil
    val links = Link(0, 2) :: Link(1, 2) :: Nil
  }

  test("Last tasks of TaskSource") {
    // Action
    val tasks = new TaskSourceDebugV1Impl().last

    // Assert
    assert(tasks.size === 1)
    assert(tasks.head === 2)
  }

  test("Sort correct") {
    // Action
    implicit val source = new TaskSourceDebugV1Impl()
    implicit val idToTask = new RightToLeftPhase()
      .rightToLeft(8)
      .map(task => task.id -> Task(task)) toMap
    val sorted = source.sortTasks

    // Assert
    assert(sorted.size === 3)
    assert(sorted.head.id === 1)
    assert(sorted.last.id === 2)
  }

  test("Sort more complicated") {
    // Action
    implicit val source = new TaskSource {
      override val links: List[Link] =
        Link(0, 2) :: Link(1, 2) :: Link(2, 4) :: Link(3, 4) :: Nil
      override val tasks: List[TaskPlain] =
        TaskPlain(0, 2) :: TaskPlain(1, 3) :: TaskPlain(2, 5) ::
        TaskPlain(3, 3) :: TaskPlain(4, 2) :: Nil
    }
    implicit val idToTask = new RightToLeftPhase()
      .rightToLeft(8)
      .map(task => task.id -> Task(task)) toMap
    val sorted = source.sortTasks map (_.id)

    // Assert
    assert(sorted.size === 5)
    assert(sorted === List(3, 1, 0, 2, 4))
  }
}
