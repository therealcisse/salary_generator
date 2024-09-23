case class Employee(
  name: String,
  taxID: String,
  salaryInformation: Employee.SalaryInformation
)

object Employee {
  case class SalaryInformation(
    gross: Int
  )

}
