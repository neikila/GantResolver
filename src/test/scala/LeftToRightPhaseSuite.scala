import phases.LeftToRightPhase
import model.{Link, TaskPlain, TaskSource}
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LeftToRightPhaseSuite extends FunSuite {

  class TaskSourceDebugV1Impl extends TaskSource {
    val tasks = TaskPlain(0, 2) :: TaskPlain(1, 3) :: TaskPlain(2, 5) :: Nil
    val links = Link(0, 2) :: Link(1, 2) :: Nil
  }

  test("Race condition") {
    implicit val source = new TaskSourceDebugV1Impl
    val result = new LeftToRightPhase leftToRight
    val last = result.toList.find { _.id == 2}
    assert(last.get.startMin.get == 3)
  }

  class TaskSourceDebugV2Impl extends TaskSource {
    val tasks = TaskPlain(0, 3) :: TaskPlain(1, 2) :: TaskPlain(2, 5) :: Nil
    val links = Link(0, 2) :: Link(1, 2) :: Nil
  }

  test("Race condition another order") {
    implicit val source = new TaskSourceDebugV2Impl
    val result = new LeftToRightPhase leftToRight
    val last = result.toList.find { _.id == 2}
    assert(last.get.startMin.get == 3)
  }

  class TaskSourceDebugV3Impl extends TaskSource {
    val tasks = TaskPlain(0, 3) :: TaskPlain(1, 2) :: TaskPlain(2, 4) :: TaskPlain(3, 5) :: Nil
    val links = Link(0, 1) :: Link(1, 3) :: Link(2, 3) :: Nil
  }

  test("Different length of branches before.") {
    implicit val source = new TaskSourceDebugV3Impl
    val result = new LeftToRightPhase leftToRight
    val last = result.toList.find { _.id == 3}
    assert(last.get.startMin.get == 5)
  }

  class TaskSourceDebugV4Impl extends TaskSource {
    val tasks = TaskPlain(0, 3) :: TaskPlain(1, 2) :: TaskPlain(2, 7) :: TaskPlain(3, 5) :: Nil
    val links = Link(0, 1) :: Link(1, 3) :: Link(2, 3) :: Nil
  }

  test("Different length of branches before. The shortest one is longer.") {
    implicit val source = new TaskSourceDebugV4Impl
    val result = new LeftToRightPhase leftToRight
    val last = result.toList.find { _.id == 3}
    assert(last.get.startMin.get == 7)
  }

  class TaskSourceDebugV5Impl extends TaskSource {
    val tasks = TaskPlain(0, 1) :: TaskPlain(1, 1) :: TaskPlain(2, 4) :: TaskPlain(3, 2) :: TaskPlain(4, 1) :: Nil
    val links = Link(0, 1) :: Link(1, 3) :: Link(2, 3) :: Link(3, 4) :: Nil
  }

  test("Updating tail.") {
    implicit val source = new TaskSourceDebugV5Impl
    val result = new LeftToRightPhase leftToRight
    val last = result.toList.find { _.id == 4}
    assert(last.get.startMin.get == 6)
  }
}
