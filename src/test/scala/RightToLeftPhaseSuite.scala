import phases.RightToLeftPhase
import model.{Link, TaskPlain, TaskSource}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RightToLeftPhaseSuite extends FunSuite {

  class TaskSourceDebugV1Impl extends TaskSource {
    val tasks = TaskPlain(0, 2) :: TaskPlain(1, 3) :: TaskPlain(2, 5) :: Nil
    val links = Link(0, 2) :: Link(1, 2) :: Nil
  }

  test("Race condition") {
    implicit val source = new TaskSourceDebugV1Impl
    val result = new RightToLeftPhase rightToLeft 8
    assert(result.toList.find { _.task.id == 1}.get.startMax.get == 0)
    assert(result.toList.find { _.task.id == 0}.get.startMax.get == 1)
  }

  class TaskSourceDebugV2Impl extends TaskSource {
    val tasks = TaskPlain(0, 3) :: TaskPlain(1, 2) :: TaskPlain(2, 5) :: Nil
    val links = Link(0, 2) :: Link(1, 2) :: Nil
  }

  test("Race condition another order") {
    implicit val source = new TaskSourceDebugV2Impl
    val result = new RightToLeftPhase rightToLeft 8
    assert(result.toList.find { _.task.id == 0}.get.startMax.get == 0)
    assert(result.toList.find { _.task.id == 1}.get.startMax.get == 1)
  }

  class TaskSourceDebugV3Impl extends TaskSource {
    val tasks = TaskPlain(0, 3) :: TaskPlain(1, 2) ::
      TaskPlain(2, 4) :: TaskPlain(3, 5) :: Nil
    val links = Link(0, 1) :: Link(1, 3) :: Link(2, 3) :: Nil
  }

  test("Different length of branches before.") {
    implicit val source = new TaskSourceDebugV3Impl
    val result = new RightToLeftPhase rightToLeft 10
    assert(result.toList.find { _.task.id == 0}.get.startMax.get == 0)
    assert(result.toList.find { _.task.id == 2}.get.startMax.get == 1)
  }

  class TaskSourceDebugV4Impl extends TaskSource {
    val tasks = TaskPlain(0, 3) :: TaskPlain(1, 2) ::
      TaskPlain(2, 7) :: TaskPlain(3, 5) :: Nil
    val links = Link(0, 1) :: Link(1, 3) :: Link(2, 3) :: Nil
  }

  test("Different length of branches before. The shortest one is longer.") {
    implicit val source = new TaskSourceDebugV4Impl
    val result = new RightToLeftPhase rightToLeft 12
    assert(result.toList.find { _.task.id == 0}.get.startMax.get == 2)
    assert(result.toList.find { _.task.id == 2}.get.startMax.get == 0)
  }

  class TaskSourceDebugV5Impl extends TaskSource {
    val tasks = TaskPlain(0, 1) :: TaskPlain(1, 1) ::
      TaskPlain(2, 4) ::
      TaskPlain(3, 2) :: TaskPlain(4, 1) :: Nil
    val links = Link(0, 1) :: Link(1, 3) :: Link(2, 3) :: Link(3, 4) :: Nil
  }

  test("Updating tail.") {
    implicit val source = new TaskSourceDebugV5Impl
    val result = new RightToLeftPhase rightToLeft 7
    assert(result.toList.find { _.task.id == 0}.get.startMax.get == 2)
    assert(result.toList.find { _.task.id == 2}.get.startMax.get == 0)
  }

  test("Check offset.") {
    implicit val source = new TaskSourceDebugV5Impl
    val result = new RightToLeftPhase() rightToLeft 0 map
      { task => task.updateEndMax(task.endMax.get + 7) }
    assert(result.toList.find { _.task.id == 0}.get.startMax.get == 2)
    assert(result.toList.find { _.task.id == 2}.get.startMax.get == 0)
  }
}
