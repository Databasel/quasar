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

import scalaz._, Scalaz._
import simulacrum.typeclass

@typeclass trait Diggable[F[_]] {
  type IT[G[_]]

  // Int is number of buckets to skip
  def digForBucket[G[_]](fg: F[IT[G]]):
      // TODO: use matryoshka.instances.fixedpoint.Nat
      StateT[Bucketing[IT, IT[G]] \/ ?, Int, F[IT[G]]]
}

object Diggable {
  type Aux[T[_[_]], F[_]] = Diggable[F] { type IT[G[_]] = T[G] }

  implicit def coproduct[T[_[_]], F[_], G[_]](
    implicit FB: Diggable.Aux[T, F], GB: Diggable.Aux[T, G]):
      Diggable.Aux[T, Coproduct[F, G, ?]] =
    new Diggable[Coproduct[F, G, ?]] {
      type IT[F[_]] = T[F]

      def digForBucket[H[_]](fg: Coproduct[F, G, IT[H]]) =
        fg.run.bitraverse(FB.digForBucket[H](_), GB.digForBucket[H](_)) ∘
          (Coproduct(_))
    }

  implicit def const[T[_[_]], A]:
      Diggable.Aux[T, Const[A, ?]] =
    new Diggable[Const[A, ?]] {
      type IT[F[_]] = T[F]

      def digForBucket[G[_]](de: Const[A, IT[G]]) = StateT.stateT(de)
    }
}
