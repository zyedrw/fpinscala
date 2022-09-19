package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter

import scala.util.control.NonFatal
import scala.{Either as _, Left as _, Right as _}

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e: E) => Left(e)
    case Right(a: A) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e: E) => Left(e)
    case Right(a: A) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e: E) => b
    case Right(a: A) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Left(e: E) => Left(e)
    case Right(a: A) => b match {
      case Left(ee: EE) => Left(ee)
      case Right(b: B) => Right(f(a, b))
    }
  }

object Either:
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case h :: t =>
        val sub = traverse(t)(f)
        sub match {
          case Right(subbs: List[B]) =>
            f(h) match {
              case Right(b: B) => Right(b :: subbs)
              case Left(e) => Left(e)
            }
          case Left(e) => Left(e)
        }

      case Nil => Right(List.empty)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(e => e)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if xs.isEmpty then
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] =
    a match {
      case Left(e: List[E]) => {
        b match {
          case Left(eb: List[E]) => Left(e.appendedAll(eb))
          case Right(b: B) => Left(e)
        }
      }
      case Right(a: A) => {
        b match {
          case Left(e) => Left(e)
          case Right(b: B) => Right(f(a,b))
        }
      }
    }

  def traverseAll[E, A, B](es: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] = ???

  def sequenceAll[E, A](es: List[Either[List[E], A]]): Either[List[E], List[A]] = ???
