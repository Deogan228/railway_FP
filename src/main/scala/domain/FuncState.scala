package domain

import monads.{*, given}

object FuncState:

  // ---------- вспомогательные функции ----------

  // поиск поезда, либо ошибка
  private def findTrain(s: OfficeState, name: String): Either[String, Train] =
    s.trains.find(_.name == name)
      .toRight(s"Ошибка: поезд $name не найден")

  // проверка свободности места
  private def checkSeat(train: Train, seat: String, cfg: TicketConfig): Either[String, Unit] =
    if FuncReader.seatAvailable(train, seat).run(cfg) then Right(())
    else Left(s"Ошибка: место $seat занято или не существует")

  // получение цены по тарифу
  private def getPrice(route: String, cls: ClassType, cfg: TicketConfig): Either[String, Double] =
    FuncReader.ticketPrice(route, cls).run(cfg)
      .toRight(s"Ошибка: нет тарифа для $route")

  // применение успешной брони к состоянию
  private def applyBooking(s: OfficeState, train: Train, seat: String,
                            cls: ClassType, price: Double, bag: Double,
                            cfg: TicketConfig): (OfficeState, Ticket) =
    val bagCost = FuncReader.baggageCost(bag).run(cfg)
    val ticket = Ticket(
      id            = s.nextTicketId,
      route         = train.route,
      classType     = cls,
      seat          = seat,
      price         = price,
      baggageWeight = bag,
      baggageCost   = bagCost
    )
    val updatedTrain  = train.copy(seats = train.seats.updated(seat, true))
    val updatedTrains = s.trains.map(t => if t.name == train.name then updatedTrain else t)
    val ns = s.copy(
      trains       = updatedTrains,
      soldTickets  = s.soldTickets :+ ticket,
      revenue      = s.revenue + price + bagCost,
      nextTicketId = s.nextTicketId + 1
    )
    (ns, ticket)

  // ---------- основная функция: линейный for ----------

  def bookTicket(trainName: String, seat: String, classType: ClassType,
                 baggageWeight: Double)(cfg: TicketConfig): State[OfficeState, Vector[String]] =
    State { s =>
      val result =
        for
          train <- findTrain(s, trainName)
          _     <- checkSeat(train, seat, cfg)
          price <- getPrice(train.route, classType, cfg)
        yield (train, price)

      result match
        case Left(err) =>
          (s, Vector(err))
        case Right((train, price)) =>
          val (ns, ticket) = applyBooking(s, train, seat, classType, price, baggageWeight, cfg)
          (ns, Vector(s"Билет #${ticket.id} оформлен: $trainName место=$seat класс=$classType цена=$price багаж=${ticket.baggageCost}"))
    }

  // ---------- остальные функции без изменений ----------

  def cancelTicket(ticketId: Int)(cfg: TicketConfig): State[OfficeState, Vector[String]] =
    State { s =>
      s.soldTickets.find(_.id == ticketId) match
        case None =>
          (s, Writer.tell(s"Ошибка: билет #$ticketId не найден").log)
        case Some(ticket) =>
          val refund = FuncReader.refundAmount(ticket).run(cfg)
          val updatedTrains = s.trains.map { t =>
            if t.route == ticket.route then t.copy(seats = t.seats.updated(ticket.seat, false))
            else t
          }
          val ns = s.copy(
            trains      = updatedTrains,
            soldTickets = s.soldTickets.filterNot(_.id == ticketId),
            revenue     = s.revenue - refund
          )
          (ns, Vector(s"Билет #$ticketId отменён, возврат=$refund (штраф=${cfg.refundPenaltyPercent * 100}%)"))
    }

  def addTrain(train: Train): State[OfficeState, Vector[String]] =
    State { s =>
      if s.trains.exists(_.name == train.name) then
        (s, Writer.tell(s"Ошибка: поезд ${train.name} уже есть").log)
      else
        val ns = s.copy(trains = s.trains :+ train)
        (ns, Writer.tell(s"Поезд ${train.name} добавлен, маршрут=${train.route} мест=${train.seats.size}").log)
    }

  def nextDay: State[OfficeState, Vector[String]] =
    State { s =>
      val ns = s.copy(soldTickets = List.empty, revenue = 0.0)
      (ns, Writer.tell(s"Новый день. Выручка за вчера: ${s.revenue}, билеты сброшены.").log)
    }