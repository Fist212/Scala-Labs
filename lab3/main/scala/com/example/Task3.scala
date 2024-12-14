package com.example

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import scala.util.Random

// Актор для вычисления интеграла на части интервала
object IntegralActor {
  case class integralCalculate(f: Double => Double, l: Double, r: Double, steps: Int, replyTo: ActorRef[Result])
  case class Result(value: Double)

  def apply(): Behavior[integralCalculate] = Behaviors.receive { (context, message) =>
    // Вычисление шага для метода численного интегрирования
    val h = (message.r - message.l) / message.steps
    // Вычисление суммы значений функции на интервале
    val result = (1 until message.steps).map(x => h * message.f(message.l + x * h)).reduce(_ + _)
    message.replyTo ! Result(result)
    Behaviors.same
  }

  // Функция для интегрирования (в данном случае x^2)
  def func(x: Double): Double = x * x
}

// Актор для суммирования частичных результатов интеграции
object SumActor {
  case class AddResult(sum: Double)

  def apply(): Behavior[AddResult] = Behaviors.setup { (context) =>
    var finalSum = 0.0  
    Behaviors.receiveMessage {
      // Обработка получения частичной суммы
      case AddResult(sum) =>
        finalSum += sum  
        context.log.info(s"Partial sum: $sum, current value: $finalSum")  
        Behaviors.same
    }
  }
}

// Главный актор, управляющий вычислением интеграла
object MainActor {
  case class StartCalculation(l: Double, r: Double, steps: Int)

  def apply(): Behavior[StartCalculation] = Behaviors.setup { (context) =>
    val integralActor = context.spawn(IntegralActor(), "integralActor")
    val sumActor = context.spawn(SumActor(), "sumActor")

    // Метод для отправки запроса на вычисление интеграла
    def sendCalculation(f: Double => Double, l: Double, r: Double, steps: Int, replyTo: ActorRef[IntegralActor.Result]): Unit = {
      integralActor ! IntegralActor.integralCalculate(f, l, r, steps, replyTo)
    }

    Behaviors.receiveMessage {
      // Обработка сообщения на запуск вычисления интеграла
      case StartCalculation(l, r, steps) =>
        val numActors = 4  
        val stepSize = (r - l) / numActors  
        (0 until numActors).foreach { i =>
          val partLeft = l + i * stepSize
          val partRight = partLeft + stepSize
          val numStepsPerPart = steps / numActors
          
          // Спавним актор для получения частичного результата и отправки его в SumActor
          val replyTo = context.spawn(Behaviors.receiveMessage[IntegralActor.Result] {
            case IntegralActor.Result(partialSum) =>
              sumActor ! SumActor.AddResult(partialSum)
              Behaviors.same
          }, s"responseActor-${i}")  
          // Отправляем запрос на вычисление интеграла для текущей части интервала
          sendCalculation(IntegralActor.func, partLeft, partRight, numStepsPerPart, replyTo)
        }
        Behaviors.same
    }
  }
}

@main def Main(): Unit = {
  val system = ActorSystem(MainActor(), "main")
  system ! MainActor.StartCalculation(0, 10, 1500)
}
