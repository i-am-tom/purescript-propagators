module Control.Monad.Network
  ( Cell

  , binary
  , make
  , read
  , unary
  , watch
  , write
  ) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Internal (kind Region)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as Ref
import Data.Semilattice.Join (class JoinSemilattice)
import Prelude

newtype Cell (r ∷ Region) (x ∷ Type)
  = Cell (STRef r { value ∷ x, onChange ∷ x → ST r Unit })

make ∷ ∀ r x. JoinSemilattice x ⇒ ST r (Cell r x)
make = map Cell (Ref.new { value: mempty, onChange: \_ → pure unit })

read ∷ ∀ r. Cell r ~> ST r
read (Cell ref) = map _.value (Ref.read ref)

watch ∷ ∀ r x. Cell r x → (x → ST r Unit) → ST r Unit
watch (Cell ref) callback = do
  { onChange, value } ← Ref.read ref

  _ ← Ref.modify _ { onChange = onChange *> callback } ref
  callback value

write ∷ ∀ r x. Eq x ⇒ JoinSemilattice x ⇒ Cell r x → x → ST r Unit
write (Cell ref) updates = do
  { onChange, value } ← Ref.read ref

  let joined = value <> updates

  when (joined /= value) do
    _ ← Ref.modify _ { value = joined } ref
    onChange joined

unary ∷ ∀ x y r. Eq y ⇒ JoinSemilattice y ⇒ (x → y) → Cell r x → Cell r y → ST r Unit
unary f x y = watch x \a → write y (f a)

binary ∷ ∀ x y z r. Eq z ⇒ JoinSemilattice z ⇒ (x → y → z) → Cell r x → Cell r y → Cell r z → ST r Unit
binary f x y z = watch x \a → watch y \b → write z (f a b)
