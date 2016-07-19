/*
 * Copyright 2014–2016 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package quasar.fp

import matryoshka._, Recursive.ops._
import scalaz._, Scalaz._

package object binder {
  /** Annotate (the original nodes of) a tree, by applying a function to the
    * "bound" nodes. The function is also applied to the bindings themselves
    * to determine their annotation.
    */
  def boundAttribute[T[_[_]]: Recursive: Corecursive, F[_]: Functor, A](
    t: T[F])(f: T[F] => A)(implicit B: Binder[F]): Cofree[F, A] = {
    def loop(t: F[T[F]], b: B.G[(T[F], Cofree[F, A])]): (T[F], Cofree[F, A]) = {
      val newB = B.bindings(t, b)(loop(_, b))
      B.subst(t, newB).fold {
        val m: F[(T[F], Cofree[F, A])] = t.map(x => loop(x.project, newB))
        val t1 = m.map(_._1).embed
        (t1, Cofree(f(t1), m.map(_._2)))
      } { case (x, _) => (x, t.embed.cata(attrK(f(x)))) }
    }
    loop(t.project, B.initial)._2
  }

  def boundCata[T[_[_]]: Recursive, F[_]: Functor, A](t: T[F])(f: F[A] => A)(implicit B: Binder[F]): A = {
    def loop(t: F[T[F]], b: B.G[A]): A = {
      val newB = B.bindings(t, b)(loop(_, b))
      B.subst(t, newB).getOrElse(f(t.map(x => loop(x.project, newB))))
    }

    loop(t.project, B.initial)
  }

  def boundParaM[T[_[_]]: Recursive, M[_]: Monad, F[_]: Traverse, A](t: T[F])(f: F[(T[F], A)] => M[A])(implicit B: Binder[F]): M[A] = {
    def loop(t: F[T[F]], b: B.G[A]): M[A] = {
      Applicative[M].sequence(B.bindings[T, M[A]](t, B.G.map(b)(_.point[M]))(s => loop(s, b)))(B.G).flatMap { newB =>
        B.subst(t, newB).cata[M[A]](
          _.point[M],
          t.traverse(x => loop(x.project, newB).map((x, _))).flatMap(f))
      }
    }

    loop(t.project, B.initial)
  }

  def boundParaS[T[_[_]]: Recursive, F[_]: Traverse, S, A](t: T[F])(f: F[(T[F], A)] => State[S, A])(implicit B: Binder[F]): State[S, A] =
    boundParaM[T, State[S, ?], F, A](t)(f)

  def boundPara[T[_[_]]: Recursive, F[_]: Functor, A](t: T[F])(f: F[(T[F], A)] => A)(implicit B: Binder[F]): A = {
    def loop(t: F[T[F]], b: B.G[A]): A = {
      val newB = B.bindings(t, b)(loop(_, b))
      B.subst(t, newB).getOrElse(f(t.map(x => (x, loop(x.project, newB)))))
    }

    loop(t.project, B.initial)
  }
}
