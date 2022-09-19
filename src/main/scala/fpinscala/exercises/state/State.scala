package fpinscala.exercises.state

import scala.annotation.tailrec


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG :
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (nextInt, nextRng) = rng.nextInt
    if (nextInt == Int.MinValue) {
      nonNegativeInt(nextRng)
    } else {
      (Math.abs(nextInt), nextRng)
    }


  def double(rng: RNG): (Double, RNG) =
  //    val (nextInt, nextRng) = nonNegativeInt(rng)
  //    (nextInt / Int.MaxValue.toDouble, nextRng)
    map(nonNegativeInt)(nextInt => nextInt / Int.MaxValue.toDouble)(rng)


  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (nextInt, nextRng) = rng.nextInt
    val (nextDouble, nextNextRng) = double(nextRng)
    ((nextInt, nextDouble), nextNextRng)


  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (nextInt, nextRng) = rng.nextInt
    val (nextDouble, nextNextRng) = double(nextRng)
    ((nextDouble, nextInt), nextNextRng)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (firstDouble, firstRng) = double(rng)
    val (secondDouble, secondRng) = double(firstRng)
    val (thirdDouble, thirdRng) = double(secondRng)
    ((firstDouble, secondDouble, thirdDouble), thirdRng)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) then (List.empty, rng)
    else {
      val (nextInt, nextRng) = rng.nextInt
      val (subList, subRng) = ints(count - 1)(nextRng)
      (nextInt :: subList, subRng)
    }


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }


  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
  //    rs.foldRight(unit(Nil: List[A]))((randA, randSubList) => map2(randA, randSubList)(_ :: _))
    rs.foldRight(unit(Nil: List[A]))( // cannot put line break before '('
      (randA, randSubList) =>
        (rng: RNG) => {
          val (a: A, nextRngForList: RNG) = randA(rng)
          val (subList: List[A], nextRng: RNG) = randSubList(nextRngForList)
          (a :: subList, nextRng)
        })

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = r(rng)
      f(a)(rng2)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => (rng => (f(a), rng)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => (rng => (f(a, b), rng))))

opaque type State[S, +A] = S => (A, S)

object State:
  extension[S, A] (underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      s => {
        val (a, nextS) = run(s)
        (f(a), nextS)
      }

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      s => {
        val (a, nextS) = run(s)
        val (b, nextNextS) = sb.run(nextS)
        (f(a, b), nextNextS)
      }

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s => {
        val (a, nextS) = run(s)
        f(a)(nextS)
      }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
