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

import scalaz._
import simulacrum.typeclass

@typeclass trait ElideBuckets[F[_]] {
  type H[_]

  type IT[G[_]]

  def purify: F[IT[H]] => H[IT[H]]
}

object ElideBuckets extends ElideBucketsInstances {
  type Aux[T[_[_]], F[_], G[_]] = ElideBuckets[F] {
    type H[A] = G[A]
    type IT[G[_]] = T[G]
  }
}

trait ElideBucketsInstances0 {
  implicit def inject[T[_[_]], F[_], G[_]](implicit F: F :<: G):
      ElideBuckets.Aux[T, F, G] =
    new ElideBuckets[F] {
      type H[A] = G[A]
      type IT[K[_]] = T[K]

      val purify = F.inj(_: F[IT[H]])
    }
}

trait ElideBucketsInstances extends ElideBucketsInstances0 {
  implicit def coproduct[T[_[_]], F[_], G[_], I[_]](
    implicit F: ElideBuckets.Aux[T, F, I], G: ElideBuckets.Aux[T, G, I]):
      ElideBuckets.Aux[T, Coproduct[F, G, ?], I] =
    new ElideBuckets[Coproduct[F, G, ?]] {
      type H[A] = I[A]
      type IT[K[_]] = T[K]

      def purify: Coproduct[F, G, T[I]] => I[T[I]] =
        _.run.fold(F.purify, G.purify)
    }
}
