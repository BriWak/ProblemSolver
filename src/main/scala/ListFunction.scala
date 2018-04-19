import scala.annotation.tailrec
import scala.util.Random

class ListFunction {

  def last[A](ls: List[A]): A = {
    ls.last
  }

  def penultimate[A](ls: List[A]): A = {
    ls.init.last
  }

  def nth[A](nth: Int, ls: List[A]): A = {
    ls(nth)
  }

  def length[A](ls: List[A]): Int = {
    ls.length
  }

  def reverse[A](ls: List[A]): List[A] = {
    ls.reverse
  }

  def isPalindrome[A](ls: List[A]): Boolean = {
    ls == ls.reverse
  }

  def flatten(ls: List[Any]): List[Any] = {
    ls.flatMap {
      case l: List[_] => flatten(l)
      case e => List(e)
    }
  }

  def compress[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h :: tail => h :: compress(tail.dropWhile(_ == h))
  }

  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls.span(_ == ls.head)
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def encode[A](ls: List[A]): List[(Int, A)] = {
    pack(ls).map(x => (x.length, x.head))
  }

  def encodeMultiples[A](ls: List[A]): List[Any] = {
    pack(ls).map(x => x.length match {
      case 1 => x.head
      case _ => (x.length, x.head)
    })
  }

  def decode[A](ls: List[(Int, A)]): List[Any] = {
    ls.flatMap(e => List.fill(e._1)(e._2))
  }

  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls.span(_ == ls.head)
      (packed.length, packed.head) :: encodeDirect(next)
    }
  }

  def duplicate[A](ls: List[A]): List[A] = {
    ls.flatMap(x => List(x,x))
  }

  def duplicateN[A](n: Int, ls: List[A]): List[A] = {
    ls.flatMap(List.fill(n)(_))
  }

  def drop[A](n: Int, ls: List[A]): List[A] = {
    ls.filter(x => (ls.indexOf(x) + 1) % n !=0)
  }

  def split[A](n: Int, ls: List[A]): (List[A],(List[A])) = {
    ls.splitAt(n)
  }

  def slice[A](start: Int, until: Int, ls: List[A]): List[A] = {
    ls.slice(start,until)
  }

  def rotate[A](n: Int, ls: List[A]): List[A] = {
    if (n >= 0) {
      val splitList = ls.splitAt(n)
      splitList._2 ::: splitList._1
    } else {
      val splitList = ls.splitAt(ls.length - n.abs)
      splitList._2 ::: splitList._1
    }
  }

  def removeAt[A](n: Int, ls: List[A]): (List[A],A) = {
    val splitList = ls.splitAt(n)
    (splitList._1 ::: splitList._2.tail, splitList._2.head)
  }

  def insertAt[A](item: A, n: Int, ls: List[A]): List[A] = {
    val splitList = ls.splitAt(n)
    splitList._1 ::: item :: splitList._2
  }

  def range(start: Int, end: Int) : List[Int] = {
    (start to end).toList
  }

  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    if (n <= 0 || n >= ls.length) Nil
    else {
      val rNum = Random.nextInt(ls.length)
      val splitList = ls.splitAt(rNum)
      val newList = splitList._1 ::: splitList._2.tail
      val h = splitList._2.head
      h :: randomSelect(n - 1, newList)
    }
  }

  def lotto(n: Int, range: Int): List[Int] = {
    val ls = (1 to range).toList

    if (n <= 0 || n >= ls.length) Nil
    else {
      val rNum = Random.nextInt(ls.length)
      val splitList = ls.splitAt(rNum)
      val newList = splitList._1 ::: splitList._2.tail
      val h = splitList._2.head
      h :: randomSelect(n - 1, newList)
    }
  }

  def randomPermute[A](ls: List[A]): List[A] = {
    val n = ls.length
    if (n <= 0) Nil
    else {
      val rNum = Random.nextInt(ls.length)
      val splitList = ls.splitAt(rNum)
      val newList = splitList._1 ::: splitList._2.tail
      val h = splitList._2.head
      h :: randomPermute(newList)
    }
  }
}

object Tester extends App {
  val l = new ListFunction
  println(l.randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
}