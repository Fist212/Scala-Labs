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
  if (password.length()>=8 && password.exists(_.isDigit) && password.exists(_.isUpper) && password.exists(_.isLower)){
  Some(true) 
  }
  else{
    None
  }
}

def goodEnoughPassword2(password:String):Either[Boolean, String] = {

  val resultCheck: Try[(Boolean,Boolean,Boolean,Boolean)] = Try {
    (password.length()>=8, password.exists(_.isDigit),password.exists(_.isUpper), password.exists(_.isLower))
  }  

  resultCheck match
  case Success((true, true, true, true)) => Left(true)
  case Success((lengthOk, hasDigit, hasUpper, hasLower)) =>
    val errors = List(
      lengthOk -> "Длина пароля менее 8 символов",
      hasDigit -> "Пароль должен содержать минимум одну цифру",
      hasUpper -> "Пароль должен иметь хотя бы одну заглавную букву",
      hasLower -> "Пароль должен иметь хотя бы одну прописную букву"
    ).collect { case (false, msg) => msg }
    Right(errors.mkString(", "))
  case Failure(exemption) => Right("С паролем полная беда")
}

def readPassword():Future[String]={

  var result = Future{
    var passwordValid = false
    var password = ""

    while(passwordValid!=true){
      println("Введите пароль. (Минимум: длина 8, одна цифра, одна заглавная буква, одна прописная буква.)")
      password = readLine()

      goodEnoughPassword2(password) match
        case Left(true) => println("Пароль верный")
          passwordValid = true
        case Right(errors) => println(errors)
    }
    password
}
  result.onComplete{
    case Success(password) => println(s"Введенный пароль: $password")
    case Failure(exception) => println(s"Произошла ошибка!")
  }
  result
}

@main def start()= {
  val res = integral(func,2.0,10.0,1000)
  println(s"Интеграл численно равен: $res")

  println(goodEnoughPassword("Password123"))

  println(goodEnoughPassword2("Password123"))
  println(goodEnoughPassword2("password"))
  println(goodEnoughPassword2("pass"))

  var password=readPassword()
  Await.result(password, Duration.Inf)
}

trait Monad[M[_]] { // Обобщенный тип, представляющий монаду. М - контейнер (Option, List...), может содержать тип A (Int, String...)
  
  def unit[A](value: A): M[A] // Метод принимает тип А и оборачивает в монаду типа M[A]

  def flatMap[A, B](A: M[A])(F: A => M[B]): M[B] // Метод принимает монаду типа M[A] и функцию F, которая преобразует монаду в тип M[B]
}

trait Functor[F[_]] {
  def map[A, B](A: F[A])(F: A => B): F[B] // Метод принимает тип F[A] и функцию F, которая преобразует тип A в B и возвращает тип F[B]
}
