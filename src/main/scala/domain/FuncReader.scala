package domain

import monads.{*, given}

// функции для чтения из конфига через Reader,
// результат заворачиваем в Writer чтобы накапливать лог
object FuncReader:

  // цена билета: лог говорит какой тариф взяли
  def ticketPrice(route: String, classType: ClassType): Reader[TicketConfig, Writer[Option[Double]]] =
    Reader { cfg =>
      cfg.tariffs.get(route) match
        case None =>
          for
            _ <- Writer.tell(s"Тариф для маршрута $route не найден")
          yield None
        case Some(t) =>
          val price = classType match
            case ClassType.Economy  => t.economy
            case ClassType.Business => t.business
          for
            _ <- Writer.tell(s"Тариф $route ($classType): $price")
          yield Some(price)
    }

  // доплата за багаж, лог — вес и итоговая сумма
  def baggageCost(weight: Double): Reader[TicketConfig, Writer[Double]] =
    Reader { cfg =>
      if weight <= 0 then
        for
          _ <- Writer.tell("Багаж: нет")
        yield 0.0
      else
        val cost = weight * cfg.baggagePerKg
        for
          _ <- Writer.tell(s"Багаж: $weight кг × ${cfg.baggagePerKg} = $cost")
        yield cost
    }

  // проверка места, лог говорит результат проверки
  def seatAvailable(train: Train, seat: String): Reader[TicketConfig, Writer[Boolean]] =
    Reader { _ =>
      train.seats.get(seat) match
        case Some(false) =>
          for _ <- Writer.tell(s"Место $seat свободно") yield true
        case Some(true) =>
          for _ <- Writer.tell(s"Место $seat занято") yield false
        case None =>
          for _ <- Writer.tell(s"Места $seat нет в поезде ${train.name}") yield false
    }
  
  // сумма возврата = (цена + багаж) * (1 - штраф)
  def refundAmount(ticket: Ticket): Reader[TicketConfig, Writer[Double]] =
    Reader { cfg =>
      val total  = ticket.price + ticket.baggageCost
      val refund = total * (1.0 - cfg.refundPenaltyPercent)
      for
        _ <- Writer.tell(s"Билет #${ticket.id}: цена=${ticket.price} багаж=${ticket.baggageCost} итого=$total")
        _ <- Writer.tell(s"Штраф ${cfg.refundPenaltyPercent * 100}%, возврат=$refund")
      yield refund
    }