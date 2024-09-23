import zio.*

trait SalarySlipGeneratorErrorManagement {
  def onError(e: SalarySlipError): UIO[Unit]
}

object SalarySlipGeneratorErrorManagement {
  def onError(e: SalarySlipError): URIO[SalarySlipGeneratorErrorManagement, Unit] =
    ZIO.serviceWithZIO(_.onError(e))

  def log(): ZLayer[Any, Nothing, SalarySlipGeneratorErrorManagement] =
    ZLayer.succeed {
      new SalarySlipGeneratorErrorManagement {
        def onError(e: SalarySlipError): UIO[Unit] = ZIO.logInfo(e.getMessage)
      }
    }

}
