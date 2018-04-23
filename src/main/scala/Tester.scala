class MathFunction {

  def isPrime(n: Int): Boolean = Range(2, n-1).forall(n % _ != 0)

  def gcd(num1: Int, num2: Int): Int = {
    println(num1, num2)
    if (num2 == 0) num1 else gcd(num2, num1 % num2)
//    val num1Divisors = Range(1, num1+1).filter(num1 % _ == 0)
//    val num2Divisors = Range(1, num2+1).filter(num2 % _ == 0)
//    num1Divisors.filter(num2Divisors.contains).last
  }
}


object Tester extends App {
  val l = new ListFunction
  val m = new MathFunction

  println(m.gcd(36, 63))
}

