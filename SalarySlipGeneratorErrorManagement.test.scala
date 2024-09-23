import zio.test.magnolia.*
import zio.test.*
import zio.*

object SalarySlipGeneratorErrorManagementSpec extends ZIOSpecDefault {
  val exampleErrorInvalidGross = SalarySlipError.InvalidGrossSalary(0)
  val exampleErrorInvalidTaxID = SalarySlipError.InvalidTaxID("1234")

  def spec =
    suite("SalarySlipGeneratorErrorManagementSpec")(
      test("log errors: gross") {
        for {
          _ <- SalarySlipGeneratorErrorManagement.onError(exampleErrorInvalidGross)

          output <- ZTestLogger.logOutput
        } yield assertTrue(output.length == 1) && assertTrue(
          output(0).message() == exampleErrorInvalidGross.getMessage
        ) && assertTrue(output(0).logLevel == LogLevel.Info)

      },
      test("log errors: taxID") {
        for {
          _ <- SalarySlipGeneratorErrorManagement.onError(exampleErrorInvalidTaxID)

          output <- ZTestLogger.logOutput
        } yield assertTrue(output.length == 1) && assertTrue(
          output(0).message() == exampleErrorInvalidTaxID.getMessage
        ) && assertTrue(output(0).logLevel == LogLevel.Info)

      }
    ).provideLayer(SalarySlipGeneratorErrorManagement.log())
}
