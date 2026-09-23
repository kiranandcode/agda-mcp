module LookupTest where

open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

module Inner where
  double : Nat → Nat
  double n = n + n

double-zero : Inner.double 0 ≡ 0
double-zero = refl

double-one : Inner.double 1 ≡ 2
double-one = lemma
  where
    open Inner
    lemma : double 1 ≡ 2
    lemma = refl
