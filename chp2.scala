object Chp2 {
  def fib(n: Int): Int = {
    if (n == 0 || n == 1) n
    else fib(n-1) + fib(n-2)
  }

  def ordered(first: Int, second: Int): Boolean = {
    if (second >= first) true
    else false
  }

  def isSorted[A](list: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= list.length) true
      else if (!ordered(list(n-1), list(n))) false
      else loop(n+1)
    }

    loop(1)
  }

  def test_isSorted(): Boolean = {
    isSorted(Array(), ordered) == true &&
    isSorted(Array(1,2,3), ordered) == true &&
    isSorted(Array(1,3,2), ordered) == false
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
