import model._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import phases._

@RunWith(classOf[JUnitRunner])
class ModelSuite extends FunSuite {


  test("TaskPlain equals true") {
    assert(TaskPlain(1, 2) === TaskPlain(1, 5))
  }

  test("TaskPlain equals false") {
    assert(TaskPlain(1, 2) !== TaskPlain(0, 5))
  }

  test("TaskWithTimeBounds equals false") {
    assert(TaskWithTimeBound(TaskPlain(1, 2)) !== TaskWithTimeBound(TaskPlain(0, 5)))
  }

  test("TaskWithTimeBounds equals true") {
    assert(TaskWithTimeBound(TaskPlain(1, 2)) === TaskWithTimeBound(TaskPlain(1, 5)))
  }
}
