import zio.test.magnolia.*
import zio.test.*
import zio.mock.*
import zio.*

object SalarySlipGeneratorSpec extends ZIOSpecDefault {
  val exampleSuccess = Employee(
    name = "Peter Mustermann",
    taxID = "12345",
    salaryInformation = Employee.SalaryInformation(gross = 500000)
  )

  val exampleInvalidGross = Employee(
    name = "Peter Mustermann",
    taxID = "12345",
    salaryInformation = Employee.SalaryInformation(gross = 0)
  )

  val exampleInvalidTaxID = Employee(
    name = "Peter Mustermann",
    taxID = "",
    salaryInformation = Employee.SalaryInformation(gross = 100)
  )

  val expectedNet = 500000 / 12

  def spec =
    suite("SalarySlipGeneratorSpec")(
      test("generate valid payslip") {
        ZIO.serviceWithZIO[SalarySlipGenerator] { generator =>
          for {
            slip <- generator.generateFor(exampleSuccess)
            netAssertion = assert(slip.net)(Assertion.equalTo(expectedNet))

          } yield netAssertion

        }
      },
      test("handle errors: invalid gross") {
        ZIO.serviceWithZIO[SalarySlipGenerator] { generator =>
          for {
            exit <- generator.generateFor(exampleInvalidGross).exit

            netAssertion = assert(exit)(
              Assertion.equalTo(
                Exit.fail(
                  SalarySlipError.InvalidGrossSalary(exampleInvalidGross.salaryInformation.gross)
                )
              )
            )

          } yield netAssertion

        }

      },
      test("handle errors: invalid taxID") {
        ZIO.serviceWithZIO[SalarySlipGenerator] { generator =>
          for {
            exit <- generator.generateFor(exampleInvalidTaxID).exit

            netAssertion = assert(exit)(
              Assertion.equalTo(Exit.fail(SalarySlipError.InvalidTaxID(exampleInvalidTaxID.taxID)))
            )

          } yield netAssertion

        }

      }
    ).provideLayer(SalarySlipGenerator.live())
}
