package plan

import monads.{*, given}
import domain.{OfficeState, TicketConfig, ClassType, Train, FuncState}

// запустить State-действие внутри IO
def runState[A](s: OfficeState, action: State[OfficeState, A]): IO[(OfficeState, A)] =
  IO.delay(action.run(s))

// печать лога одной строкой
def printLog(log: Vector[String]): IO[Unit] =
  if log.isEmpty then IO.pure(())
  else IO.writeLine(log.map("  " + _).mkString("\n"))

def readInt(prompt: String, fallback: Int): IO[Int] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim.toIntOption.getOrElse(fallback)

def readDouble(prompt: String, fallback: Double): IO[Double] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim.toDoubleOption.getOrElse(fallback)

def readStr(prompt: String): IO[String] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim

def showTrainsAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  val lines = s.trains.zipWithIndex.map { case (t, i) =>
    val free = t.seats.count { case (_, occupied) => !occupied }
    s"  ${i + 1}. ${t.name} | ${t.route} | свободно: $free/${t.seats.size}"
  }
  val info = lines.mkString("\n")
  IO.writeLine(if info.isEmpty then "  поездов нет" else info).map(_ => s)

def showTicketsAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  val lines = s.soldTickets.map { t =>
    s"  #${t.id} ${t.route} ${t.classType} место=${t.seat} цена=${t.price} багаж=${t.baggageCost}"
  }
  val info = lines.mkString("\n")
  IO.writeLine(if info.isEmpty then "  билетов нет" else info).map(_ => s)

def bookTicketAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    _         <- showTrainsAction(s, cfg)
    trainNum  <- readInt("Номер поезда: ", -1)
    trainName  = if trainNum >= 1 && trainNum <= s.trains.size
                 then s.trains(trainNum - 1).name
                 else ""
    _         <- s.trains.find(_.name == trainName) match
                   case Some(t) =>
                     val freeSeats = t.seats.filter { case (_, occupied) => !occupied }.keys.toList.sorted
                     IO.writeLine(s"Свободные места: ${freeSeats.mkString(", ")}")
                   case None =>
                     IO.writeLine("Поезд не найден")
    seat      <- readStr("Место: ")
    clsStr    <- readStr("Класс (economy/business): ")
    cls        = if clsStr.toLowerCase.startsWith("b") then ClassType.Business else ClassType.Economy
    baggage   <- readDouble("Вес багажа кг (0 если нет): ", 0.0)
    res       <- runState(s, FuncState.bookTicket(trainName, seat, cls, baggage)(cfg))
    (ns, log)  = res
    _         <- printLog(log)
  yield ns

def cancelTicketAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    _        <- showTicketsAction(s, cfg)
    id       <- readInt("Номер билета: ", -1)
    res      <- runState(s, FuncState.cancelTicket(id)(cfg))
    (ns, log) = res
    _        <- printLog(log)
  yield ns

def addTrainAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    name   <- readStr("Название поезда: ")
    route  <- readStr("Маршрут (напр. Moscow-SPb): ")
    nRows  <- readInt("Количество рядов: ", 10)
    seats   = makeSeats(nRows)
    train   = Train(name, route, seats)
    res    <- runState(s, FuncState.addTrain(train))
    (ns, log) = res
    _      <- printLog(log)
  yield ns

// создание карты мест: nRows рядов по 4 места (A, B, C, D)
def makeSeats(nRows: Int): Map[String, Boolean] =
  val pairs = for
    r <- 1 to nRows
    c <- Seq("A", "B", "C", "D")
  yield s"$r$c" -> false
  pairs.toMap

def nextDayAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    res      <- runState(s, FuncState.nextDay)
    (ns, log) = res
    _        <- printLog(log)
  yield ns