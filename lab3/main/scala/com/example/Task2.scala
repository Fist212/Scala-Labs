package com.example

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.example.GreeterMain.SayHello
import scala.util.Random

// Актор, суммирующий числа
object AddingServer{
    case class AddMessage(a:Int, b:Int, replyTo:ActorRef[Int])
    // Метод определяет поведение актора, а именно суммирует числа.
    def apply():Behavior[AddMessage] = Behaviors.receive{ (context, message)=> 
        context.log.info("Sum {} + {} = {}",message.a, message.b, message.a + message.b)
        // Отправляем сообщение с результатом
        message.replyTo ! message.a + message.b
        Behaviors.same
    }
}

// Актор, принимающий числа пользователя
object AddingClient {
    // ActorRef[AddingServer.AddMessage] - ссылка на актор AddingServer, ожидающего сообщение типа AddMessage
    def apply(ref: ActorRef[AddingServer.AddMessage]): Behavior[Int] = Behaviors.setup { context =>
    def sendNumbers(count: Int): Behavior[Int] = {
      if (count == 0) {
        Behaviors.stopped // Завершаем работу после 5 сообщений
      } else {
        val a = Random.nextInt(50)
        val b = Random.nextInt(50)
        context.log.info("Client sent {} and {}", a, b)
        ref ! AddingServer.AddMessage(a, b, context.self)
        Behaviors.receiveMessage { message =>
          context.log.info("Client has received a response: {}\n", message)
          sendNumbers(count - 1) 
        }
      }
    }
    sendNumbers(5) 
  }
}

object AddingSystem{
    def apply():Behavior[Unit] = Behaviors.setup { (context) =>
        val adder = context.spawn(AddingServer(), "adder")
        val client = context.spawn(AddingClient(adder), "client")
        Behaviors.empty
    }
}
@main def AddingMain():Unit =
    val system = ActorSystem(AddingSystem(),"system")


