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

package quasar.ejson

import quasar.Predef._
import quasar.fp._, Helpers._

import matryoshka._
import org.scalacheck._
import org.specs2.scalaz._
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._

class EJsonSpecs extends Spec {
  /** Long value that can safely be represented in any possible backend
    * (including those using JavaScript.)
    */
  val SafeInt: Gen[Long] = Gen.choose(-1000L, 1000L)

  implicit val arbitraryCommon: Arbitrary ~> (Arbitrary ∘ Common)#λ =
    new (Arbitrary ~> (Arbitrary ∘ Common)#λ) {
      def apply[α](arb: Arbitrary[α]): Arbitrary[Common[α]] = {
        Arbitrary(Gen.oneOf(
          Gen.listOf(arb.arbitrary).map(Arr(_)),
          Null[α]().point[Gen],
          Arbitrary.arbitrary[Boolean].map(Bool[α](_)),
          Arbitrary.arbitrary[String].map(Str[α](_)),
          Arbitrary.arbitrary[BigDecimal].map(Dec[α](_))))
      }
    }

  implicit val arbitraryObj: Arbitrary ~> (Arbitrary ∘ Obj)#λ =
    new (Arbitrary ~> (Arbitrary ∘ Obj)#λ) {
      def apply[α](arb: Arbitrary[α]): Arbitrary[Obj[α]] = {
        implicit def a: Arbitrary[α] = arb
        Arbitrary(Gen.listOf(Arbitrary.arbitrary[(String, α)]).map(l =>
          Obj(l.toListMap)))
      }
    }

  implicit val arbitraryExtension: Arbitrary ~> (Arbitrary ∘ Extension)#λ =
    new (Arbitrary ~> (Arbitrary ∘ Extension)#λ) {
      def apply[α](arb: Arbitrary[α]): Arbitrary[Extension[α]] = {
        implicit def a: Arbitrary[α] = arb
        Arbitrary(Gen.oneOf(
          Gen.listOf(Arbitrary.arbitrary[(α, α)]).map(Map(_)),
          Arbitrary.arbitrary[scala.Byte].map(Byte[α](_)),
          Arbitrary.arbitrary[scala.Char].map(Char[α](_)),
          Arbitrary.arbitrary[BigInt].map(Int[α](_))))
      }
    }

  checkAll(equal.laws[Common[String]])
  checkAll(traverse.laws[Common])

  checkAll(equal.laws[Obj[String]])
  checkAll(traverse.laws[Obj])

  checkAll(equal.laws[Extension[String]])
  checkAll(traverse.laws[Extension])
}
