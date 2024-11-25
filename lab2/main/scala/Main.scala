implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
import scala.io.StdIn.readLine
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Try, Failure, Success}

def integral(f:Double => Double, l:Double, r:Double, steps:Int):Double = {
  var h = (r-l)/steps
  (1 until steps).map(x=>h*f(l+x*h)).sum
  }

def func(x:Double):Double = x*x

def goodEnoughPassword(password:String):Option[Boolean] = {
  val filtersColl: Seq[String=>Boolean] = 
  Seq(p => p.length()>=8, 
      p => p.exists(_.isDigit), 
      p => p.exists(_.isUpper), 
      p => p.exists(_.isLower))

  val combinedFilter = filtersColl.reduce((f1,f2) => x=> f1(x) && f2(x))
  
  //если результат не None, применяем .map(_ => true), чтобы вернуть Some(true)
  Some(password).filter(combinedFilter).map(_ => true)
}


def goodEnoughPassword2(password:String):Either[Boolean, String] = {
      Try{
      Seq((password.length()>=8) -> "Длина пароля менее 8 символов", 
      password.exists(_.isDigit) -> "Пароль должен содержать минимум одну цифру", 
      password.exists(_.isUpper)-> "Пароль должен иметь хотя бы одну заглавную букву", 
      password.exists(_.isLower) -> "Пароль должен иметь хотя бы одну прописную букву").collect{ 
      case (false, errorMessage) => Right(errorMessage) }.headOption.getOrElse(Left(true))  
      }
      match {
        case Success(result) => result
        case Failure(_) => Right("Какая-то ошибка")
  }

}

def readPassword(): Future[String] = {
  Future {
    readPasswordFromUser()
  }
}

def readPasswordFromUser(): String = {
  println("Введите пароль. (Минимум: длина 8, одна цифра, одна заглавная буква, одна прописная буква.)")
  val password = readLine()

  goodEnoughPassword2(password) match {
    case Left(_) => password
    case Right(errors) =>
      println(errors)
      readPasswordFromUser()  
  }
}

@main def start()= {
  val res = integral(func,2.0,10.0,1000)
  println("Первое задание")
  println(s"Интеграл численно равен: $res")

  println("")
  println("Второе задание Option")
  println(goodEnoughPassword("Password123"))
  println(goodEnoughPassword("Pass"))

  println("")
  println("Третье задание Try")
  println(goodEnoughPassword2("Password123"))
  println(goodEnoughPassword2("password"))
  println(goodEnoughPassword2("pass"))

  println("")
  println("Четверное задание Future")
  var password=readPassword()
  var resultPassword = Await.result(password, Duration.Inf)
  println(s"Успешный пароль: $resultPassword")
}

trait Monad[M[_]] { // Обобщенный тип, представляющий монаду. М - контейнер (Option, List...), может содержать тип A (Int, String...)
  
  def unit[A](value: A): M[A] // Метод принимает тип А и оборачивает в монаду типа M[A]

  def flatMap[A, B](A: M[A])(F: A => M[B]): M[B] // Метод принимает монаду типа M[A] и функцию F, которая преобразует монаду в тип M[B]
}

trait Functor[F[_]] {
  def map[A, B](A: F[A])(F: A => B): F[B] // Метод принимает тип F[A] и функцию F, которая преобразует тип A в B и возвращает тип F[B]
}
