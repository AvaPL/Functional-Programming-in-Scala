package chapter12.traverse

import chapter12.applicative1.Applicative
import chapter3.{Branch, Leaf, Tree}

trait Traverse[F[_]] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(identity)
}

object Traverse {
  val list: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[List[B]] = {
      import applicative.{unit, map2}
      fa.map(f).foldRight(unit(List.empty[B]))(map2(_, _)(_ :: _))
    }
  }

  val option: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[Option[B]] = {
      import applicative.{unit, map}
      fa.map(f).fold(unit(Option.empty[B]))(map(_)(Some(_)))
    }
  }

  val tree: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[Tree[B]] = {
      import applicative.{map, map2}
      fa.map(f).fold(map(_)(Leaf(_): Tree[B]))(map2(_, _)(Branch(_, _)))
    }
  }
}