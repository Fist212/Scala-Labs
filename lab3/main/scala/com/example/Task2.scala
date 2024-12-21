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
object AddingClient:
    def apply(serverRef: ActorRef[AddingServer.AddMessage]): Behavior[Int] = Behaviors.setup { (context) =>
        def sendMessage(): Unit = {
            val a = Random.between(0,1000)
            val b = Random.between(0,1000)
            serverRef ! AddingServer.AddMessage(a, b, context.self)
        }
        sendMessage()
        Behaviors.receiveMessage{ message => 
            context.log.info(message.toString() + '\n')
            sendMessage()
            Behaviors.same
        }
    }

object MyActorSystem:
    case class Message()
    def apply(): Behavior[Message] = Behaviors.setup{ (context) =>
        val server = context.spawn(AddingServer(),"server")
        val client = context.spawn(AddingClient(server),"client")
        
        Behaviors.empty
    }
@main def AddingMain():Unit =
    val system = ActorSystem(MyActorSystem(),"system")



