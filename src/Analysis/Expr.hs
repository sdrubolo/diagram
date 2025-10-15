module Expr where

data Propagation a =
    Propagation
      { top :: a -> a
      , bottom :: a -> a
      }

instance Monoid (Propagation a) where
  mempty = Propagation {top=id, bottom=id}

instance Semigroup (Propagation a) where
  (<>) a b =
    Propagation
      { top =  top_b . top_a
      , bottom = bottom_b . bottom_a
      }
    where
      Propagation{top=top_a, bottom=bottom_a} = a
      Propagation{top=top_b, bottom=bottom_b} = b

data Expr a b =
    Expr
      { propagation :: Propagation a
      , expr :: a -> a -> b -> b
      }

instance Monoid (Expr a b) where
  mempty = Expr { propagation=mempty, expr = \_ _ y -> y }

instance Semigroup (Expr a b) where
  (<>) a b =
    Expr
      { propagation= prop_a <> prop_b
      , expr = \x y -> let a_top = top_b . top_a
                           b_top = bottom_a . a_top
                           elems_a = expr_a (a_top x) (b_top x)
                           elems_b = expr_b (b_top y) (bottom_b $ b_top y)
                     in elems_a . elems_b
      }
    where
      Expr{propagation=prop_a@Propagation{top=top_a, bottom=bottom_a}, expr=expr_a} = a
      Expr{propagation=prop_b@Propagation{top=top_b, bottom=bottom_b}, expr=expr_b} = b
