package miniscala


object nSet {

  sealed abstract class nList[T]
  case class Nil[T]() extends nList[T]
  case class Cons[T](x: T, xs: nList[T]) extends nList[T]
  type nSet[A] = nList[A]

  def length[T](xs: nList[T]): Int = xs match {
    case Nil() => 0
    case Cons(_, ys) => 1 + length(ys)
  }
  def append[T](xs: nList[T], x: T): nList[T] = xs match {
    case Nil() => Cons(x, Nil())
    case Cons(y, ys) => Cons(y, append(ys, x))
  }
  def makeEmpty[A](): nSet[A] = Nil()
  def isEmpty[A](set: nSet[A]): Boolean = size[A](set) == 0
  def size[A](set: nSet[A]): Int = set match {
    case Nil() => 0
    case Cons(_, yset) => 1 + size(yset)
  }
  def add[A](set: nSet[A], x: A): nSet[A] = set match {
    case Nil() => Cons(x, Nil())
    case Cons(y,ys) => Cons(y,append(ys,x))
  }
  def contains[A](set: nSet[A], x: A): Boolean = set match {
    case Nil() => false
    case Cons(y,yset) =>
      if (y == x)
        true
      else contains(yset,x)
  }
  def remove[A](set: nSet[A], x: A): nSet[A] = set match {
    case Nil() => Nil()
    case Cons(y, yset) => if(x==y) yset else Cons(y,remove(yset,x))
  }
  def union[A](set1: nSet[A], set2: nSet[A]): nSet[A] = set1 match {
    case Nil() => set2
    case Cons(y,yset) =>
      if (contains(set2,y)) {
        union(yset,set2)
      } else {
        union(yset,add(set2,y))
      }
  }
  def intersection[A](set1: nSet[A], set2: nSet[A]): nSet[A] = set1 match {
    case Nil() => Nil()
    case Cons(y,yset) =>
      if (contains(set2,y)) {
        Cons(y,intersection(yset,set2))
      } else {
        intersection(yset,set2)
      }
  }
  def difference[A](set1: nSet[A], set2: nSet[A]): nSet[A] = set1 match {
    case Nil() => Nil()
    case Cons(y,yset) =>
      if (contains(set2,y)) {
        difference(yset,set2)
      } else {
        Cons(y,difference(yset,set2))
      }
  }

}
