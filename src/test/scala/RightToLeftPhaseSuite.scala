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
    val result = new RightToLeftPhase rightToLeft(new TaskSourceDebugV1Impl, 8)
    assert(result.toList.find { _.task.id == 1}.get.startMax.get == 0)
    assert(result.toList.find { _.task.id == 0}.get.startMax.get == 1)
  }

  class TaskSourceDebugV2Impl extends TaskSource {
    val tasks = TaskPlain(0, 3) :: TaskPlain(1, 2) :: TaskPlain(2, 5) :: Nil
    val links = Link(0, 2) :: Link(1, 2) :: Nil
  }

  test("Race condition another order") {
    val result = new RightToLeftPhase rightToLeft(new TaskSourceDebugV2Impl, 8)
    assert(result.toList.find { _.task.id == 0}.get.startMax.get == 0)
    assert(result.toList.find { _.task.id == 1}.get.startMax.get == 1)
  }

  class TaskSourceDebugV3Impl extends TaskSource {
    val tasks = TaskPlain(0, 3) :: TaskPlain(1, 2) ::
      TaskPlain(2, 4) :: TaskPlain(3, 5) :: Nil
    val links = Link(0, 1) :: Link(1, 3) :: Link(2, 3) :: Nil
  }

  test("Different length of branches before.") {
    val result = new RightToLeftPhase rightToLeft(new TaskSourceDebugV3Impl, 10)
    assert(result.toList.find { _.task.id == 0}.get.startMax.get == 0)
    assert(result.toList.find { _.task.id == 2}.get.startMax.get == 1)
  }

  class TaskSourceDebugV4Impl extends TaskSource {
    val tasks = TaskPlain(0, 3) :: TaskPlain(1, 2) ::
      TaskPlain(2, 7) :: TaskPlain(3, 5) :: Nil
    val links = Link(0, 1) :: Link(1, 3) :: Link(2, 3) :: Nil
  }

  test("Different length of branches before. The shortest one is longer.") {
    val result = new RightToLeftPhase rightToLeft(new TaskSourceDebugV4Impl, 12)
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
    val result = new RightToLeftPhase rightToLeft(new TaskSourceDebugV5Impl, 7)
    assert(result.toList.find { _.task.id == 0}.get.startMax.get == 2)
    assert(result.toList.find { _.task.id == 2}.get.startMax.get == 0)
  }

  test("Check offset.") {
    val result = new RightToLeftPhase rightToLeft(new TaskSourceDebugV5Impl, 0) map
      { task => task.updateEndMax(task.endMax.get + 7) }
    assert(result.toList.find { _.task.id == 0}.get.startMax.get == 2)
    assert(result.toList.find { _.task.id == 2}.get.startMax.get == 0)
  }
}
