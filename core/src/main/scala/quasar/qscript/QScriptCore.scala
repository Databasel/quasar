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

package quasar.qscript

import quasar.Predef._
import quasar.fp._
import quasar.namegen._
import quasar.qscript.MapFuncs._

import matryoshka._
import monocle.macros.Lenses
import scalaz._, Scalaz._
import shapeless.{Fin, Nat, Sized, Succ}

sealed abstract class QScriptCore[T[_[_]], A] {
  def src: A
}

/** A data-level transformation.
  */
@Lenses final case class Map[T[_[_]], A](src: A, f: FreeMap[T])
    extends QScriptCore[T, A]

/** Performs a reduction over a dataset, with the dataset partitioned by the
  * result of the MapFunc. So, rather than many-to-one, this is many-to-fewer.
  *
  * `bucket` partitions the values into buckets based on the result of the
  * expression, `reducers` applies the provided reduction to each expression,
  * and repair finally turns those reduced expressions into a final value.
  *
  * @group MRA
  */
@Lenses final case class Reduce[T[_[_]], A, N <: Nat](
  src: A,
  bucket: FreeMap[T],
  reducers: Sized[List[ReduceFunc[FreeMap[T]]], Succ[N]],
  repair: Free[MapFunc[T, ?], Fin[Succ[N]]])
    extends QScriptCore[T, A]

/** Sorts values within a bucket. This could be represented with
  *     LeftShift(Map(_.sort, Reduce(_ :: _, ???))
  * but backends tend to provide sort directly, so this avoids backends having
  * to recognize the pattern. We could provide an algebra
  *     (Sort :+: QScript)#λ => QScript
  * so that a backend without a native sort could eliminate this node.
  */
@Lenses final case class Sort[T[_[_]], A](
  src: A,
  bucket: FreeMap[T],
  order: List[(FreeMap[T], SortDir)])
    extends QScriptCore[T, A]

/** Eliminates some values from a dataset, based on the result of FilterFunc.
  */
@Lenses final case class Filter[T[_[_]], A](src: A, f: FreeMap[T])
    extends QScriptCore[T, A]

@Lenses final case class Take[T[_[_]], A](src: A, from: FreeQS[T], count: FreeQS[T])
    extends QScriptCore[T, A]

@Lenses final case class Drop[T[_[_]], A](src: A, from: FreeQS[T], count: FreeQS[T])
    extends QScriptCore[T, A]

object QScriptCore {
  implicit def equal[T[_[_]]: EqualT]: Delay[Equal, QScriptCore[T, ?]] =
    new Delay[Equal, QScriptCore[T, ?]] {
      def apply[A](eq: Equal[A]) =
        Equal.equal {
          case (Map(a1, f1), Map(a2, f2)) => f1 ≟ f2 && eq.equal(a1, a2)
          case (Reduce(a1, b1, f1, r1), Reduce(a2, b2, f2, r2)) =>
            b1 ≟ b2 && f1 ≟ f2 && r1 ≟ r2 && eq.equal(a1, a2)
          case (Sort(a1, b1, o1), Sort(a2, b2, o2)) =>
            b1 ≟ b2 && o1 ≟ o2 && eq.equal(a1, a2)
          case (Filter(a1, f1), Filter(a2, f2)) => f1 ≟ f2 && eq.equal(a1, a2)
          case (Take(a1, f1, c1), Take(a2, f2, c2)) => eq.equal(a1, a2) && f1 ≟ f2 && c1 ≟ c2
          case (Drop(a1, f1, c1), Drop(a2, f2, c2)) => eq.equal(a1, a2) && f1 ≟ f2 && c1 ≟ c2
          case (_, _) => false
        }
    }

  implicit def traverse[T[_[_]]]: Traverse[QScriptCore[T, ?]] =
    new Traverse[QScriptCore[T, ?]] {
      def traverseImpl[G[_]: Applicative, A, B](
        fa: QScriptCore[T, A])(
        f: A => G[B]) =
        fa match {
          case Map(a, func)       => f(a) ∘ (Map[T, B](_, func))
          case Reduce(a, b, func, repair) => f(a) ∘ (Reduce(_, b, func, repair))
          case Sort(a, b, o)              => f(a) ∘ (Sort(_, b, o))
          case Filter(a, func)            => f(a) ∘ (Filter(_, func))
          case Take(a, from, c)           => f(a) ∘ (Take(_, from, c))
          case Drop(a, from, c)           => f(a) ∘ (Drop(_, from, c))
        }
    }

  implicit def show[T[_[_]]: ShowT]: Delay[Show, QScriptCore[T, ?]] =
    new Delay[Show, QScriptCore[T, ?]] {
      def apply[A](s: Show[A]): Show[QScriptCore[T, A]] =
        Show.show {
          case Map(src, mf) => Cord("Map(") ++
            s.show(src) ++ Cord(",") ++
            mf.show ++ Cord(")")
          case Reduce(a, b, red, rep) => Cord("Reduce(") ++
            s.show(a) ++ Cord(",") ++
            b.show ++ Cord(",") ++
            red.show ++ Cord(",") ++
            rep.show ++ Cord(")")
          case Sort(a, b, o) => Cord("Sort(") ++
            s.show(a) ++ Cord(",") ++
            b.show ++ Cord(",") ++
            o.show ++ Cord(")")
          case Filter(a, func) => Cord("Filter(") ++
            s.show(a) ++ Cord(",") ++
            func.show ++ Cord(")")
          case Take(a, f, c) => Cord("Take(") ++
            s.show(a) ++ Cord(",") ++
            f.show ++ Cord(",") ++
            c.show ++ Cord(")")
          case Drop(a, f, c) => Cord("Drop(") ++
            s.show(a) ++ Cord(",") ++
            f.show ++ Cord(",") ++
            c.show ++ Cord(")")
        }
    }

  implicit def mergeable[T[_[_]]: Corecursive: EqualT]:
      Mergeable.Aux[T, QScriptCore[T, Unit]] =
    new Mergeable[QScriptCore[T, Unit]] {
      type IT[F[_]] = T[F]

      def mergeSrcs(
        left: FreeMap[IT],
        right: FreeMap[IT],
        p1: QScriptCore[IT, Unit],
        p2: QScriptCore[IT, Unit]) =
        OptionT((p1, p2) match {
          case (Map(_, m1), Map(_, m2)) => for {
            lname <- freshName("leftMap")
            rname <- freshName("rightMap")
          } yield {
            val lf = Free.roll[MapFunc[IT, ?], Unit](ProjectField(UnitF[IT], StrLit(lname)))
            val rf = Free.roll[MapFunc[IT, ?], Unit](ProjectField(UnitF[IT], StrLit(rname)))

            SrcMerge[QScriptCore[IT, Unit], FreeMap[IT]](Map((), Free.roll[MapFunc[IT, ?], Unit](
              ConcatMaps(
                Free.roll[MapFunc[IT, ?], Unit](MakeMap(StrLit(lname), rebase(m1, left))),
                Free.roll[MapFunc[IT, ?], Unit](MakeMap(StrLit(rname), rebase(m2, right)))))),
              lf, rf).some
          }
          case (t1, t2) if t1 ≟ t2 =>
            state(SrcMerge[QScriptCore[IT, Unit], FreeMap[IT]](t1, UnitF, UnitF).some)
          case (Reduce(_, bucket1, func1, rep1), Reduce(_, bucket2, func2, rep2)) => {
            val mapL = rebase(bucket1, left)
            val mapR = rebase(bucket2, right)

            state((mapL ≟ mapR).option(
              SrcMerge[QScriptCore[IT, Unit], FreeMap[IT]](
                Reduce((), mapL, func1 // ++ func2 // TODO: append Sizeds
                  , rep1),
                UnitF,
                UnitF)))
          }
          case (_, _) =>
            state((p1 ≟ p2).option(SrcMerge(p1, left, right)))
        })
    }

  implicit def diggable[T[_[_]]: Corecursive]:
      Diggable.Aux[T, QScriptCore[T, ?]] =
    new Diggable[QScriptCore[T, ?]] {
      type IT[G[_]] = T[G]

      def digForBucket[G[_]](fg: QScriptCore[T, IT[G]]) =
        fg match {
          case Reduce(_, _, _, _)
             | Sort(_, _, _) =>
            StateT(s => (s + 1, fg).right)
          case _ => IndexedStateT.stateT(fg)
      }
    }

  implicit def normalizable[T[_[_]]: Recursive: Corecursive: EqualT]:
      Normalizable[QScriptCore[T, ?]] =
    new Normalizable[QScriptCore[T, ?]] {
      def normalize = new (QScriptCore[T, ?] ~> QScriptCore[T, ?]) {
        def apply[A](qc: QScriptCore[T, A]) = qc match {
          case Map(src, f) => Map(src, normalizeMapFunc(f))
          case Reduce(src, bucket, reducers, repair) =>
            Reduce(src, normalizeMapFunc(bucket), reducers.map(_.map(normalizeMapFunc(_))), normalizeMapFunc(repair))
          case Sort(src, bucket, order) =>
            Sort(src, normalizeMapFunc(bucket), order.map(_.leftMap(normalizeMapFunc(_))))
          case Filter(src, f) => Filter(src, normalizeMapFunc(f))
          case Take(src, from, count) =>
            Take(
              src,
              from.mapSuspension(Normalizable[QScriptInternal[T, ?]].normalize),
              count.mapSuspension(Normalizable[QScriptInternal[T, ?]].normalize))
          case Drop(src, from, count) =>
            Drop(
              src,
              from.mapSuspension(Normalizable[QScriptInternal[T, ?]].normalize),
              count.mapSuspension(Normalizable[QScriptInternal[T, ?]].normalize))
        }
      }
    }
}
