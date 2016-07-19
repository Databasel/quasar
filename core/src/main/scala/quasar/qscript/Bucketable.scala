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

import scalaz._

// TODO: make this a @typeclass (currently errors when we do)
trait Bucketable[F[_]] {
  type IT[G[_]]

  def applyBucket[G[_]: Functor](
    ft: F[IT[G]], inner: IT[G])(
    f: (IT[G],
        IT[G],
        SrcMerge[ThetaJoin[IT, IT[G]], FreeMap[IT]] =>
            (IT[G], FreeMap[IT], FreeMap[IT])) =>
          QSState[(IT[G], FreeMap[IT], FreeMap[IT])])(
    implicit TJ: ThetaJoin[IT, ?] :<: G):
      QSState[(IT[G], FreeMap[IT], FreeMap[IT])]
}

object Bucketable {
  type Aux[T[_[_]], F[_]] = Bucketable[F] { type IT[G[_]] = T[G] }

  implicit def coproduct[T[_[_]], F[_], H[_]](
    implicit FB: Bucketable.Aux[T, F], HB: Bucketable.Aux[T, H]):
      Bucketable.Aux[T, Coproduct[F, H, ?]] =
    new Bucketable[Coproduct[F, H, ?]] {
      type IT[F[_]] = T[F]

      def applyBucket[G[_]: Functor](
        ft: Coproduct[F, H, IT[G]], inner: IT[G])(
        f: (IT[G],
          IT[G],
          SrcMerge[ThetaJoin[IT, IT[G]], FreeMap[IT]] =>
          (IT[G], FreeMap[IT], FreeMap[IT])) =>
        QSState[(IT[G], FreeMap[IT], FreeMap[IT])])(
        implicit TJ: ThetaJoin[IT, ?] :<: G) =
        ft.run.fold(FB.applyBucket(_, inner)(f), HB.applyBucket(_, inner)(f))
    }
}
