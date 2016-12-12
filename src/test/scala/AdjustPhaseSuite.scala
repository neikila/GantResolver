import model._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import phases._

@RunWith(classOf[JUnitRunner])
class AdjustPhaseSuite extends FunSuite {


  test("ALAP, NoMatter, ASAP") {
    // Arrange
    val tasks = TaskWithTimeBound(TaskPlain(0, 2, TaskType.ALAP), Some(0), Some(4)) ::
      TaskWithTimeBound(TaskPlain(1, 3), Some(2), Some(7)) :: Nil
    val links = Link(0, 1) :: Link(1, 2) :: Nil

    // Action
    val task = Task(Some(5), Some(10), TaskWithTimeBound(TaskPlain(2, 5, TaskType.ASAP), Some(5), Some(12)))
    val (updatedMap, others) = new AdjustPhase(tasks, links).checkBefore(Map(task.id -> task), tasks map (t => t.id -> t) toMap)
    val updated = (updatedMap values).toList

    // Assert
    val toTest = updated.sortBy(_.id)

    assert(toTest.head.id == 0)
    assert(toTest.head.start.get == 0)
    assert(toTest.head.end.get == 2)

    assert(toTest.last.id == 1)
    assert(toTest.last.start.get == 2)
    assert(toTest.last.end.get == 5)
  }

  test("ALAP, NoMatter, ASAP, ALAP") {
    val tasks = TaskWithTimeBound(TaskPlain(0, 2, TaskType.ALAP), Some(0), Some(4)) ::
      TaskWithTimeBound(TaskPlain(1, 3), Some(2), Some(7)) ::
      TaskWithTimeBound(TaskPlain(2, 5, TaskType.ASAP), Some(5), Some(12)) ::
      TaskWithTimeBound(TaskPlain(3, 3, TaskType.ALAP), Some(10), Some(15)) :: Nil
    val links = Link(0, 1) :: Link(1, 2) :: Link(2, 3) :: Nil

    val toTest = new AdjustPhase(tasks, links).execute()

    assert(toTest.head.id == 0)
    assert(toTest.head.start.get == 0)
    assert(toTest.head.end.get == 2)

    assert(toTest(1).id == 1)
    assert(toTest(1).start.get == 2)
    assert(toTest(1).end.get == 5)

    assert(toTest(2).id == 2)
    assert(toTest(2).start.get == 5)
    assert(toTest(2).end.get == 10)

    assert(toTest.last.id == 3)
    assert(toTest.last.start.get == 12)
    assert(toTest.last.end.get == 15)
  }
}
