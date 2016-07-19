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
import quasar.ejson, ejson.EJson

import matryoshka._, Recursive.ops._
import monocle.macros.Lenses
import scalaz._, Scalaz._

sealed abstract class QScriptBucket[T[_[_]], A] {
  def src: A
}

@Lenses final case class GroupBy[T[_[_]], A](
  src: A,
  values: FreeMap[T],
  bucket: FreeMap[T])
    extends QScriptBucket[T, A]

@Lenses final case class LeftShiftBucket[T[_[_]], A](
  src: A,
  struct: FreeMap[T],
  repair: JoinFunc[T],
  bucketShift: FreeMap[T])
    extends QScriptBucket[T, A]

@Lenses final case class SquashBucket[T[_[_]], A](
  src: A)
    extends QScriptBucket[T, A]

object QScriptBucket {
  implicit def equal[T[_[_]]: EqualT]: Delay[Equal, QScriptBucket[T, ?]] =
    new Delay[Equal, QScriptBucket[T, ?]] {
      def apply[A](eq: Equal[A]) =
        Equal.equal {
          case (GroupBy(a1, v1, b1), GroupBy(a2, v2, b2)) =>
            eq.equal(a1, a2) && v1 ≟ v2 && b1 ≟ b2
          case (LeftShiftBucket(a1, s1, r1, b1), LeftShiftBucket(a2, s2, r2, b2)) =>
            eq.equal(a1, a2) && s1 ≟ s2 && r1 ≟ r2 && b1 ≟ b2
          case (SquashBucket(a1), SquashBucket(a2)) =>
            eq.equal(a1, a2)
          case (_, _) => false
        }
    }

  implicit def traverse[T[_[_]]]: Traverse[QScriptBucket[T, ?]] =
    new Traverse[QScriptBucket[T, ?]] {
      def traverseImpl[G[_], A, B](
        fa: QScriptBucket[T, A])(
        f: A => G[B])(
        implicit G: Applicative[G]):
          G[QScriptBucket[T, B]] = fa match {
        case GroupBy(src, values, bucket) =>
          f(src) ∘ (GroupBy(_, values, bucket))
        case LeftShiftBucket(src, struct, repair, bucket) =>
          f(src) ∘ (LeftShiftBucket(_, struct, repair, bucket))
        case SquashBucket(src) =>
          f(src) ∘ (SquashBucket(_))
      }
    }

  implicit def show[T[_[_]]: ShowT]: Delay[Show, QScriptBucket[T, ?]] =
    new Delay[Show, QScriptBucket[T, ?]] {
      def apply[A](sh: Show[A]): Show[QScriptBucket[T, A]] =
        Show.show {
          case GroupBy(a, v, b) => Cord("GroupBy(") ++
            sh.show(a) ++ Cord(",") ++
            v.show ++ Cord(",") ++
            b.show ++ Cord(")")
          case LeftShiftBucket(a, s, r, b) => Cord("LeftShiftBucket(") ++
            sh.show(a) ++ Cord(",") ++
            s.show ++ Cord(",") ++
            r.show ++ Cord(",") ++
            b.show ++ Cord(")")
          case SquashBucket(a) => Cord("SquashBucket(") ++
            sh.show(a) ++ Cord(")")
        }
    }

  implicit def mergeable[T[_[_]]: Corecursive: EqualT]:
      Mergeable.Aux[T, QScriptBucket[T, Unit]] =
    new Mergeable[QScriptBucket[T, Unit]] {
      type IT[F[_]] = T[F]

      def mergeSrcs(
        left: FreeMap[IT],
        right: FreeMap[IT],
        p1: QScriptBucket[IT, Unit],
        p2: QScriptBucket[IT, Unit]) =
        OptionT(state((p1 ≟ p2).option(SrcMerge(p1, left, right))))
    }

  implicit def diggable[T[_[_]]: Corecursive]:
      Diggable.Aux[T, QScriptBucket[T, ?]] =
    new Diggable[QScriptBucket[T, ?]] {
      implicit val QB =
        scala.Predef.implicitly[QScriptBucket[T, ?] :<: Bucketing[T, ?]]

      type IT[G[_]] = T[G]

      def digForBucket[G[_]](fg: QScriptBucket[T, IT[G]]) =
        StateT(s =>
          if (s ≟ 0)
            QB.inj(fg match {
              case LeftShiftBucket(src, struct, repair, bucket) =>
                LeftShiftBucket(src, rebase(struct, bucket), repair, bucket)
              case x => x
            }).left
          else
            (s - 1, fg).right)
    }

  implicit def bucketable[T[_[_]]: Corecursive]:
      Bucketable.Aux[T, QScriptBucket[T, ?]] =
    new Bucketable[QScriptBucket[T, ?]] {
      type IT[G[_]] = T[G]

      def applyBucket[G[_]: Functor](
        ft: QScriptBucket[IT, IT[G]], inner: IT[G])(
        f: (IT[G],
            IT[G],
            SrcMerge[ThetaJoin[IT, IT[G]], FreeMap[IT]] =>
                (IT[G], FreeMap[IT], FreeMap[IT])) =>
              QSState[(IT[G], FreeMap[IT], FreeMap[IT])])(
        implicit TJ: ThetaJoin[IT, ?] :<: G) =
        ft match {
          case GroupBy(src, _, bucket) =>
            f(
              src,
              inner,
              { case SrcMerge(src, mfl, mfr) =>
                (TJ.inj(src).embed, rebase(bucket, mfl), mfr)
              })
          case LeftShiftBucket(src, struct, _, bucket) =>
            f(
              src,
              inner,
              { case SrcMerge(src, mfl, mfr) =>
                (TJ.inj(src).embed, rebase(rebase(struct, bucket), mfl), mfr)
              })
          case SquashBucket(src) =>
            StateT.stateT((inner, Free.roll(Nullary(CommonEJson.inj(ejson.Null[T[EJson]]()).embed)), UnitF)) // singleton provenance - one big bucket
        }
    }

  implicit def elideBuckets[T[_[_]]: Recursive, F[_]: Functor](
    implicit QC: QScriptCore[T, ?] :<: F, SP: SourcedPathable[T, ?] :<: F):
      ElideBuckets.Aux[T, QScriptBucket[T, ?], F] =
    new ElideBuckets[QScriptBucket[T, ?]] {
      type H[A] = F[A]
      type IT[G[_]] = T[G]

      def purify: QScriptBucket[T, T[H]] => H[IT[H]] = {
        case GroupBy(src, values, _) => QC.inj(Map(src, values))
        case LeftShiftBucket(src, struct, repair, _) => SP.inj(LeftShift(src, struct, repair))
        case SquashBucket(src) => src.project
      }
    }
}
