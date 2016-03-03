module Stack (
  Stack,
  push,
  peek, peekOrElse,
  pop, popOrElse
) where

type Stack a = [a]

push :: a -> Stack a -> Stack a
push = (:)

peek :: Stack a -> (Stack a, a)
peek [] = error "peek: stack is empty"
peek as@(a : _) = (as, a)

peekOrElse :: Stack a -> a -> (Stack a, a)
peekOrElse [] a = ([], a)
peekOrElse as@(a : _) _ = (as, a)

pop :: Stack a -> (Stack a, a)
pop [] = error "pop: stack is empty"
pop (a : as) = (as, a)

popOrElse :: Stack a -> a -> (Stack a, a)
popOrElse [] a = ([], a)
popOrElse (a : as) _ = (as, a)
