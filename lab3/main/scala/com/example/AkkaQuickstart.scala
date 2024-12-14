//#full-example
package com.example

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.example.GreeterMain.SayHello

//#greeter-actor
//Greeter — это актор, который принимает сообщения о приветствии
object Greeter {
  //Сообщение для актора, содержащее имя whom и ссылку на отправителя ActorRef[Greeted]
  final case class Greet(whom: String, replyTo: ActorRef[Greeted])
  //Ответное сообщение, которое отправляется после приветствия
  final case class Greeted(whom: String, from: ActorRef[Greet])

  //Метод определяет поведение актора при приеме сообщений (возвращает Behavior[Greet], Greet - тип сообщения )
  def apply(): Behavior[Greet] = Behaviors.receive { (context, message) =>
    // Логируем приветствие
    context.log.info("Hello {}!", message.whom)
    //#greeter-send-messages
    //Отправляем ответное сообщение отправителю
    message.replyTo ! Greeted(message.whom, context.self)
    //#greeter-send-messages
    //Не изменяем поведение актора
    Behaviors.same
  }
}
//#greeter-actor

//#greeter-bot
//GreeterBot — это актор, который отправляет несколько приветствий
object GreeterBot {

  // Начальное поведение бота с максимальным количеством приветствий
  def apply(max: Int): Behavior[Greeter.Greeted] = {
    bot(0, max) // Начинаем с нуля
  }

  // Рекурсивное поведение бота для обработки сообщений 
  private def bot(greetingCounter: Int, max: Int): Behavior[Greeter.Greeted] =
    Behaviors.receive { (context, message) =>
      val n = greetingCounter + 1 // Увеличиваем счетчик
      context.log.info("Greeting {} for {}", n, message.whom)
      if (n == max) { // Если достигнут лимит приветствий
        Behaviors.stopped // Останавливаем актора
      } else {
        // Отправляем новое сообщение Greeter
        message.from ! Greeter.Greet(message.whom, context.self)
        // Возвращаем поведение с обновленным счетчиком
        bot(n, max)
      }
    }
}
//#greeter-bot

//#greeter-main
//GreeterMain —  это главный объект, который управляет акторами
object GreeterMain {

  // Сообщение для запуска приветствия
  final case class SayHello(name: String)

  // Начальное поведение GreeterMain
  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      //#create-actors
      // Создаем актор Greeter
      val greeter = context.spawn(Greeter(), "greeter")
      //#create-actors

      Behaviors.receiveMessage { message =>
        //#create-actors
        // Создаем нового GreeterBot для каждого нового сообщения
        val replyTo = context.spawn(GreeterBot(max = 3), message.name)
        //#create-actors
        // Отправляем сообщение Greeter с именем и ссылкой на GreeterBot
        greeter ! Greeter.Greet(message.name, replyTo)
        // Повторяем текущее поведение
        Behaviors.same
      }
    }
}
//#greeter-main

//#main-class
// Главный объект приложения
object AkkaQuickstart extends App {
  //#actor-system
  // Создаем ActorSystem, который запускает GreeterMain
  val greeterMain: ActorSystem[GreeterMain.SayHello] = ActorSystem(GreeterMain(), "AkkaQuickStart")
  //#actor-system

  //#main-send-messages
  // Отправляем сообщение для запуска приветствия
  greeterMain ! SayHello("Charles")
  //#main-send-messages
}
//#main-class
//#full-example

