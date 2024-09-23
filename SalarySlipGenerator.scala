import zio.*
import zio.stream.*
import scala.util.control.NonFatal

trait SalarySlipGenerator {
  def generateFor(e: Employee): ZIO[Any, SalarySlipError, SalarySlip]
}

object SalarySlipGenerator {
  def generateFor[R](e: Employee): RIO[R & SalarySlipGenerator, SalarySlip] =
    ZIO.serviceWithZIO[SalarySlipGenerator](_.generateFor(e))

  def generateFor: ZSink[
    SalarySlipGenerator & SalarySlipGeneratorErrorManagement,
    Throwable,
    Employee,
    Nothing,
    Unit
  ] =
    ZSink.foreach { (e: Employee) =>
      SalarySlipGenerator.generateFor(e).onError {
        _.failureOrCause.fold(
          {
            case NonFatal(e @ SalarySlipError.InvalidGrossSalary(_)) =>
              SalarySlipGeneratorErrorManagement.onError(e)
            case NonFatal(e @ SalarySlipError.InvalidTaxID(_)) =>
              SalarySlipGeneratorErrorManagement.onError(e)
            case _ => ZIO.unit

          },
          Exit.failCause
        )
      }

    }

  def generateFor(
    employees: List[Employee]
  ): RIO[SalarySlipGenerator & SalarySlipGeneratorErrorManagement, Chunk[SalarySlip]] =

    val res = ZStream(employees*)
      .mapZIOParUnordered(n = sys.runtime.availableProcessors) { e =>
        SalarySlipGenerator.generateFor(e).onError {
          _.failureOrCause.fold(
            {
              case NonFatal(e @ SalarySlipError.InvalidGrossSalary(_)) =>
                SalarySlipGeneratorErrorManagement.onError(e)
              case NonFatal(e @ SalarySlipError.InvalidTaxID(_)) =>
                SalarySlipGeneratorErrorManagement.onError(e)
              case _ => ZIO.unit

            },
            Exit.failCause
          )
        }
      }

    res.runCollect

  def live(): ZLayer[Any, Nothing, SalarySlipGenerator] =
    ZLayer.succeed {
      new SalarySlipGenerator {
        def generateFor(e: Employee): ZIO[Any, SalarySlipError, SalarySlip] =
          if e.salaryInformation.gross <= 0 then
            ZIO.fail(SalarySlipError.InvalidGrossSalary(e.salaryInformation.gross))
          else if !isValidTaxID(e.taxID) then ZIO.fail(SalarySlipError.InvalidTaxID(e.taxID))
          else ZIO.succeed(SalarySlip(employee = e, net = calcNet(e.salaryInformation.gross)))

        private def isValidTaxID(id: String): Boolean = !id.isEmpty

        private def calcNet(gross: Int): Int = (gross / 12).toInt

      }

    }

}

enum SalarySlipError extends RuntimeException {
  case InvalidGrossSalary(gross: Int)
  case InvalidTaxID(id: String)

  override def getMessage(): String = this match {
    case InvalidGrossSalary(gross) => s"Invalid gross salary: $gross"
    case InvalidTaxID(id)          => s"Invalid Tax ID: $id"

  }
}
