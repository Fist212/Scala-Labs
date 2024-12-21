package com.example

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

// Актор, вычисляющий интеграл
object IntegralActor {
  case class Calculate(start: Double, end: Double, numSteps: Int, replyTo: ActorRef[Result])
  case class Result(value: Double)

  def apply(): Behavior[Calculate] = Behaviors.receive { (context, message) =>
    val step = (message.end - message.start) / message.numSteps
    val intermediateSum = (1 until message.numSteps).map { i =>
      val x = message.start + i * step
      f(x)
    }.sum
    val result = (f(message.start) + f(message.end)) / 2 + intermediateSum
    val integral = result * step
    message.replyTo ! Result(integral)
    Behaviors.same
  }

  private def f(x: Double): Double = x * x
}

// Актор, суммирующий частитчные результаты
object SumActor {
  case class AddResult(sum: Double, remainingResults: Int)
  case class FinalResult(result: Double)

  def apply(): Behavior[AddResult] = Behaviors.setup { context =>
    var totalSum = 0.0
    var resultsLeft = 0

    Behaviors.receiveMessage {
      case AddResult(sum, remainingResults) =>
        totalSum += sum
        resultsLeft = remainingResults
        context.log.info(s"Partial sum: $sum. Current total: $totalSum. Remaining results: $resultsLeft")

        if (resultsLeft == 0) {
          context.log.info(s"Final result: $totalSum")
        }
        Behaviors.same
    }
  }
}

// Главный актор
object MainActor {
  case class StartCalculation(start: Double, end: Double, steps: Int)

  def apply(): Behavior[StartCalculation] = Behaviors.setup { context =>
    val integralActor = context.spawn(IntegralActor(), "IntegralActor")
    val sumActor = context.spawn(SumActor(), "SumActor")

    def distributeCalculations(start: Double, end: Double, steps: Int, numActors: Int): Unit = {
      val stepSize = (end - start) / steps
      val stepsPerActor = steps / numActors
      val rangePerActor = (end - start) / numActors
      var remainingResults = numActors

      (0 until numActors).foreach { i =>
        val actorStart = start + i * rangePerActor
        val actorEnd = actorStart + rangePerActor

        val replyTo = context.spawn(Behaviors.receiveMessage[IntegralActor.Result] {
          case IntegralActor.Result(partialSum) =>
            remainingResults -= 1
            sumActor ! SumActor.AddResult(partialSum, remainingResults)
            Behaviors.same
        }, s"ResponseActor-$i")

        integralActor ! IntegralActor.Calculate(actorStart, actorEnd, stepsPerActor, replyTo)
      }
    }

    Behaviors.receiveMessage {
      case StartCalculation(start, end, steps) =>
        val numActors = 4
        distributeCalculations(start, end, steps, numActors)
        Behaviors.same
    }
  }
}

@main def Main(): Unit = {
  val system = ActorSystem(MainActor(), "MainActorSystem")
  system ! MainActor.StartCalculation(0, 5, 1000)
}