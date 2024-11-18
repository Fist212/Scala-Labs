def multiHelloEvenOdd(n:Int) = {
  for (i <- 1 to n) if (i%2 == 0) println(s"hello $i") else println(s"hello ${n - i}")
}

def splitByIndex(numbers: Seq[Int]): (Seq[Int], Seq[Int]) = {
  val evenIndex = numbers.zipWithIndex.filter((_, index) => index%2 == 0).map(_._1)
  val oddsIndex = numbers.zipWithIndex.filter((_, index) => index%2 != 0).map(_._1)

  (evenIndex,oddsIndex)
}

def getMaxElemet(number:Seq[Int]):Int = {
  number.reduce((a, b) => if (a>b) a else b)
}

def sumSplit(number:Seq[Int]):(Int,Int)={
  var sumPositive = 0
  var sumNegative = 0 
  for(num <- number){
      num match{
    case n:Int if n>0 => sumPositive += n
    case n:Int if n<0 => sumNegative += n
    case _ =>
  }
  }
  (sumPositive,sumNegative)
}

def compose(xEven:Int=>Boolean, toString:Boolean=>String):Int=>String = 
  x=>toString(xEven(x))

@main def start() = {
  ///////////////////////////////////////////////////
  print(multiHelloEvenOdd(10))

  ///////////////////////////////////////////////////
  val (evens,odds) = splitByIndex(5 to 10)
  println(s"Элементы с четным индексом $evens")
  println(s"Элементы с нечетным индексом $odds")

  ///////////////////////////////////////////////////
  val nums = Seq(12,451,-15,120)
  val address = getMaxElemet
  println("Максимальный элемент " + address(nums))

  ///////////////////////////////////////////////////
  val num = Seq(1,-3,3,10,0,15,-100)
  val (positive, negative) = sumSplit(num)
  println(s"Сумма положительных чисел $positive")
  println(s"Сумма отрицательных чисел $negative")

 ///////////////////////////////////////////////////
  val xEven = (x:Int) => x%2==0 
  val toString = (x:Boolean) => if (x) "Четное" else "Нечетное"
  val num2 = 10
  println(num2 +" "+ compose(xEven,toString)(num2))
}