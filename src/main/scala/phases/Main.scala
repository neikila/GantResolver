package phases

import model.{Task, TaskSource, TaskWithTimeBound}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.languageFeature.postfixOps

/**
  * Created by k.neyman on 10.12.2016.
  */
object Main {
  def calculate(implicit taskSource: TaskSource): Future[List[Task]] = {
    val leftToRightFuture = Future { new LeftToRightPhase leftToRight }
    val rightToLeftFuture = Future { new RightToLeftPhase rightToLeft 0 }

    for {
      l2rResult <- leftToRightFuture
      r2lResult <- rightToLeftFuture
      withBounds <- Future {
        val last = l2rResult.maxBy(_.endMin.get)
        val r2lWithOffset = r2lResult map { task =>
          task.updateEndMax(task.endMax.map { _ + last.endMin.get })
        }
        zip(l2rResult, r2lWithOffset)
      }
      result <- Future { new AdjustPhase(withBounds, taskSource.links).execute() }
    } yield result
  }

  def zip(min: Iterable[TaskWithTimeBound], max: Iterable[TaskWithTimeBound]) = {
    (min.toList.sortBy(_.id) zip max.toList.sortBy(_.id)) map {
      case(minTask, maxTask) => TaskWithTimeBound(minTask.task, minTask.startMin, maxTask.endMax)
    }
  }
}
