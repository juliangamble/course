{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Parser where

import Course.Core
import Course.Person
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.List
import Course.Optional
import Data.Char

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

type Input = Chars

data ParseError =
  UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | Failed
  deriving Eq


instance Show ParseError where
  show UnexpectedEof =
    "Unexpected end of stream"
  show (ExpectedEof i) =
    stringconcat ["Expected end of stream, but got >", show i, "<"]
  show (UnexpectedChar c) =
    stringconcat ["Unexpected character: ", show [c]]
  show Failed =
    "Parse failed"

data ParseResult a =
  ErrorResult ParseError
  | Result Input a
  deriving Eq

instance Show a => Show (ParseResult a) where
  show (ErrorResult e) =
    show e
  show (Result i a) =
    stringconcat ["Result >", hlist i, "< ", show a]

-- Function to determine is a parse result is an error.
isErrorResult ::
  ParseResult a
  -> Bool
isErrorResult (ErrorResult _) =
  True
isErrorResult (Result _ _) =
  False

data Parser a = P {
  parse :: Input -> ParseResult a
}

-- | Produces a parser that always fails with @UnexpectedChar@ using the given character.
unexpectedCharParser ::
  Char
  -> Parser a
unexpectedCharParser c =
  P (\_ -> ErrorResult (UnexpectedChar c))

-- | Return a parser that always succeeds with the given value and consumes no input.
--
-- >>> parse (valueParser 3) "abc"
-- Result >abc< 3
valueParser ::
  a
  -> Parser a
--valueParser =
--  error "todo: Course.Parser#valueParser"
--valueParser a =
valueParser value =
--  P (`Result` a)
-- constructor P
-- quite similar to pure
--  _undefined
--  P (undefined undefined)
--  P (_undefined undefined)
--  P (_undefined a)
--  P (ParseResult a)
--  P undefined
--  P _undefined
--  P (\input -> undefined)
--  P (\input -> Result undefined undefined)
--  P (\input -> Result undefined value)
  P (\input -> Result input value)


-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse failed "abc")
-- True
failed ::
  Parser a
failed =
--  error "todo: Course.Parser#failed"
--  P (\_ -> ErrorResult Failed)
  P (\_ -> ErrorResult Failed)


-- | Return a parser that succeeds with a character off the input or fails with an error if the input is empty.
--
-- >>> parse character "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse character "")
-- True
character ::
  Parser Char
character =
--  error "todo: Course.Parser#character"
--  P (\s -> case s of Nil -> ErrorResult UnexpectedEof
--                     (c:.r) -> Result r c)
--  P _undefined
  P somefunc

somefunc ::
  Input -> ParseResult Char
somefunc Nil = ErrorResult UnexpectedEof
somefunc (h:.t)  = Result t h


-- | Return a parser that maps any succeeding result with the given function.
--
-- >>> parse (mapParser succ character) "amz"
-- Result >mz< 'b'
--
-- >>> parse (mapParser (+10) (valueParser 7)) ""
-- Result >< 17
mapParser ::
  (a -> b)
  -> Parser a
  -> Parser b
--mapParser =
--  error "todo: Course.Parser#mapParser"
--Parsers are functors - they can be mapped on

mapParser f (P p) =
  P (\input -> case p input of 
                 ErrorResult e -> ErrorResult e
                 Result r a -> Result r (f a))

-- | This is @mapParser@ with the arguments flipped.
-- It might be more helpful to use this function if you prefer this argument order.
flmapParser ::
  Parser a
  -> (a -> b)
  -> Parser b
flmapParser =
  flip mapParser

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "abc"
-- Result >bc< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "a"
-- Result >< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "xabc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "")
-- True
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "x")
-- True

-- parser fits in the class of monads

bindParser ::
  (a -> Parser b)
  -> Parser a
  -> Parser b
--bindParser =
--  error "todo: Course.Parser#bindParser"
bindParser f (P p) =
  P (\i -> case p i of
             Result r a -> parse (f a) r
             ErrorResult e -> ErrorResult e)

-- | This is @bindParser@ with the arguments flipped.
-- It might be more helpful to use this function if you prefer this argument order.
flbindParser ::
  Parser a
  -> (a -> Parser b)
  -> Parser b
flbindParser =
  flip bindParser

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- /Tip:/ Use @bindParser@ or @flbindParser@.
--
-- >>> parse (character >>> valueParser 'v') "abc"
-- Result >bc< 'v'
--
-- >>> isErrorResult (parse (character >>> valueParser 'v') "")
-- True
(>>>) ::
  Parser a
  -> Parser b
  -> Parser b
--(>>>) =
--  error "todo: Course.Parser#(>>>)"
p >>> q =
  bindParser (const q) p

-- | Return a parser that tries the first parser for a successful value.
--
--   * If the first parser succeeds then use this parser.
--
--   * If the first parser fails, try the second parser.
--
-- >>> parse (character ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (failed ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (character ||| valueParser 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (failed ||| valueParser 'v') "abc"
-- Result >abc< 'v'
(|||) ::
  Parser a
  -> Parser a
  -> Parser a
--(|||) =
--  error "todo: Course.Parser#(|||)"

P p1 ||| P p2 =
  P (\s -> let v = p1 s
           in if isErrorResult v
                then
                  p2 s
                else
                  v)

{-
P p ||| P q =
  P (\input -> case p input of
        ErrorResult _ -> q input
            r@(Result{}) -> r)
-}

  -- @ 'as pattern' - assign r to this entire expression - 
  -- and do pattern matching on this constructor


infixl 3 |||

-- | Return a parser that continues producing a list of values from the given parser.
--
-- /Tip:/ Use @list1@, @valueParser@ and @(|||)@.
--
-- >>> parse (list character) ""
-- Result >< ""
--
-- >>> parse (list digit) "123abc"
-- Result >abc< "123"
--
-- >>> parse (list digit) "abc"
-- Result >abc< ""
--
-- >>> parse (list character) "abc"
-- Result >< "abc"
--
-- >>> parse (list (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (list (character *> valueParser 'v')) ""
-- Result >< ""
list ::
  Parser a
  -> Parser (List a)
--list =
list p =
--  error "todo: Course.Parser#list"
-- list k =
--  list1 k ||| valueParser Nil
-- the parser that produces 0 or many from P
-- 0 or any is the same things and 1 or any and zero
  list1 p ||| valueParser Nil



  -- seems like a copy out to define it in terms of the subsequent function
  -- DEFECT - implementing this function requires implementing list1 below
  -- and Apply below - before you can get  > parse (list (character *> valueParser 'v')) "abc"
  -- to work
  -- so this is the incorrect order. 


-- | Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
--
-- /Tip:/ Use @bindParser@, @list@ and @valueParser@.
--
-- >>> parse (list1 (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (list1 (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (list1 (character *> valueParser 'v')) "")
-- True
list1 ::
  Parser a
  -> Parser (List a)
--list1 =
--  error "todo: Course.Parser#list1"
--list1 k =
list1 p =
{-
  flbindParser k (\k' ->
  flbindParser (list k) (\kk' ->
  valueParser (k' :. kk')))
-}
  --- run p
  -- 1 or many is the same as one of a and then 0 or any call it b a:.b
{-
  flbindParser p (\a -> 
    flbindParser (list p) (\b ->
      valueParser (a:.b)))
-}

  do  a <- p -- 1 of
      b <- list p -- 0/many of
      pure (a:.b) -- cons them!
    -- go home


-- | Return a parser that produces a character but fails if
--
--   * The input is empty.
--
--   * The character does not satisfy the given predicate.
--
-- /Tip:/ The @bindParser@, @unexpectedCharParser@ and @character@ functions will be helpful here.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy ::
  (Char -> Bool)
  -> Parser Char
--satisfy =
--  error "todo: Course.Parser#satisfy"
satisfy p =
--  bindParser (\c ->
--    if p c then valueParser c else unexpectedCharParser c) character

  character `flbindParser`
  lift3 iiif unexpectedCharParser valueParser p

iiif :: x -> x -> Bool -> x
iiif f _ False = f
iiif _ t True = t

-- | Return a parser that produces the given character but fails if
--
--   * The input is empty.
--
--   * The produced character is not equal to the given character.
--
-- /Tip:/ Use the @satisfy@ function.
is ::
  Char -> Parser Char
--is =
--  error "todo: Course.Parser#is"
is c =
  satisfy (== c)

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * The input is empty.
--
--   * The produced character is not a digit.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isDigit@ functions.
digit ::
  Parser Char
--digit =
--  error "todo: Course.Parser#digit"
digit =
  satisfy isDigit

-- | Return a parser that produces zero or a positive integer but fails if
--
--   * The input is empty.
--
--   * The input does not produce a valid series of digits
--
-- /Tip:/ Use the @bindParser@, @valueParser@, @list1@, @read@ and @digit@
-- functions.
-- >>> parse natural "123"
-- Result >< 123
--
-- >>> parse natural "123ab"
-- Result >ab< 123
--
-- >>> isErrorResult (parse natural "abc")
-- True
--
-- >>> isErrorResult (parse natural "")
-- True
natural ::
  Parser Int
--natural =
--  error "todo: Course.Parser#natural"
natural =
--  bindParser (\k -> case read k of Empty        -> failed
--                                   Full h -> valueParser h) (list digit)

  list digit >>= \d ->
  case read d of
    Empty -> failed
    Full i -> pure i

--
-- | Return a parser that produces a space character but fails if
--
--   * The input is empty.
--
--   * The produced character is not a space.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isSpace@ functions.
space ::
  Parser Char
--space =
--  error "todo: Course.Parser#space"
space =
  satisfy isSpace

-- | Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--
--   * The input is empty.
--
--   * The first produced character is not a space.
--
-- /Tip:/ Use the @list1@ and @space@ functions.
spaces1 ::
  Parser Chars
--spaces1 =
--  error "todo: Course.Parser#spaces1"
spaces1 =
  list1 space

-- | Return a parser that produces a lower-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not lower-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isLower@ functions.
lower ::
  Parser Char
--lower =
--  error "todo: Course.Parser#lower"
lower =
  satisfy isLower

-- | Return a parser that produces an upper-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not upper-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isUpper@ functions.
upper ::
  Parser Char
--upper =
--  error "todo: Course.Parser#upper"
upper =
  satisfy isUpper

-- | Return a parser that produces an alpha character but fails if
--
--   * The input is empty.
--
--   * The produced character is not alpha.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isAlpha@ functions.
alpha ::
  Parser Char
--alpha =
--  error "todo: Course.Parser#alpha"
alpha =
  satisfy isAlpha

-- | Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
--
-- /Tip:/ Use @bindParser@ and @valueParser@.
-- /Tip:/ Optionally use @List#foldRight@. If not, an explicit recursive call.
--
-- >>> parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef"
-- Result >def< "axC"
--
-- >>> isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
-- True
sequenceParser ::
  List (Parser a)
  -> Parser (List a)
--sequenceParser =
--  error "todo: Course.Parser#sequenceParser"
--sequenceParser Nil =
--  valueParser Nil
--sequenceParser (h:.t) =
--  flbindParser h (\a ->
--  flbindParser (sequenceParser t) (\as ->
--  valueParser (a :. as)))
sequenceParser = sequence

-- | Return a parser that produces the given number of values off the given parser.
-- This parser fails if the given parser fails in the attempt to produce the given number of values.
--
-- /Tip:/ Use @sequenceParser@ and @List.replicate@.
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- Result >ef< "ABCD"
--
-- >>> isErrorResult (parse (thisMany 4 upper) "ABcDef")
-- True
thisMany ::
  Int
  -> Parser a
  -> Parser (List a)
  -- replicateM :: 
  --Monad m => Int -> m a -> m [a]
--  Applicative m => Int -> m a -> List [a]

--thisMany =
--  error "todo: Course.Parser#thisMany"
thisMany n p =
  sequenceParser (replicate n p)
-- this is a list of parsers- that is in a list that many times
-- we use this because we have five or more lowercase characters


-- | Write a parser for Person.age.
--
-- /Age: positive integer/
--
-- /Tip:/ Equivalent to @natural@.
--
-- >>> parse ageParser "120"
-- Result >< 120
--
-- >>> isErrorResult (parse ageParser "abc")
-- True
--
-- >>> isErrorResult (parse ageParser "-120")
-- True
ageParser ::
  Parser Int
--ageParser =
--  error "todo: Course.Parser#ageParser"
ageParser =
  natural

-- | Write a parser for Person.firstName.
-- /First Name: non-empty string that starts with a capital letter and is followed by zero or more lower-case letters/
--
-- /Tip:/ Use @bindParser@, @valueParser@, @upper@, @list@ and @lower@.
--
-- >>> parse firstNameParser "Abc"
-- Result >< "Abc"
--
-- >>> isErrorResult (parse firstNameParser "abc")
-- True
firstNameParser ::
  Parser Chars
--firstNameParser =
--  error "todo: Course.Parser#firstNameParser"
firstNameParser =
--  flbindParser upper (\c ->
--  flbindParser (list lower) (\cs ->
--  valueParser (c :. cs)))

  P (\input -> case parse upper input of 
    ErrorResult e -> ErrorResult e
    Result j a -> parse  ((\u ->
      list lower `flbindParser` \ls ->
      pure (u:.ls)) a) j)

{-
  do u <- upper
     ls <- list lower
     pure (u:.ls)
-}

-- | Write a parser for Person.surname.
--
-- /Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters./
--
-- /Tip:/ Use @bindParser@, @valueParser@, @upper@, @thisMany@, @lower@ and @list@.
--
-- >>> parse surnameParser "Abcdef"
-- Result >< "Abcdef"
--
-- >>> isErrorResult (parse surnameParser "Abc")
-- True
--
-- >>> isErrorResult (parse surnameParser "abc")
-- True
surnameParser ::
  Parser Chars
--surnameParser =
--  error "todo: Course.Parser#surnameParser"
surnameParser =
  flbindParser upper (\c ->
  flbindParser (thisMany 5 lower) (\cs ->
  flbindParser (list lower) (\t ->
  valueParser (c :. cs ++ t))))

-- | Write a parser for Person.smoker.
--
-- /Smoker: character that must be @'y'@ or @'n'@/
--
-- /Tip:/ Use @is@ and @(|||)@./
--
-- >>> parse smokerParser "yabc"
-- Result >abc< 'y'
--
-- >>> parse smokerParser "nabc"
-- Result >abc< 'n'
--
-- >>> isErrorResult (parse smokerParser "abc")
-- True
smokerParser ::
  Parser Char
--smokerParser =
--  error "todo: Course.Parser#smokerParser"
smokerParser =
  is 'y' ||| is 'n'

-- | Write part of a parser for Person#phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
--
-- /Phone: string of digits, dots or hyphens .../
--
-- /Tip:/ Use @list@, @digit@, @(|||)@ and @is@.
--
-- >>> parse phoneBodyParser "123-456"
-- Result >< "123-456"
--
-- >>> parse phoneBodyParser "123-4a56"
-- Result >a56< "123-4"
--
-- >>> parse phoneBodyParser "a123-456"
-- Result >a123-456< ""
phoneBodyParser ::
  Parser Chars
--phoneBodyParser =
--  error "todo: Course.Parser#phoneBodyParser"
phoneBodyParser =
  list (digit ||| is '.' ||| is '-')

-- | Write a parser for Person.phone.
--
-- /Phone: ... but must start with a digit and end with a hash (#)./
--
-- /Tip:/ Use @bindParser@, @valueParser@, @digit@, @phoneBodyParser@ and @is@.
--
-- >>> parse phoneParser "123-456#"
-- Result >< "123-456"
--
-- >>> parse phoneParser "123-456#abc"
-- Result >abc< "123-456"
--
-- >>> isErrorResult (parse phoneParser "123-456")
-- True
--
-- >>> isErrorResult (parse phoneParser "a123-456")
-- True
phoneParser ::
  Parser Chars
--phoneParser =
--  error "todo: Course.Parser#phoneParser"
phoneParser =
{-  flbindParser digit (\d ->
  flbindParser phoneBodyParser (\z ->
  flbindParser (is '#') (\_ ->
  valueParser (d :. z))))
-}
{-
  do
    d <- digit
    b <- phoneBodyParser
    is '#'
    pure (d:.b)
-}
  (:.) <$> digit <*> phoneBodyParser <* is '#'

-- | Write a parser for Person.
--
-- /Tip:/ Use @bindParser@,
--            @valueParser@,
--            @(>>>)@,
--            @spaces1@,
--            @ageParser@,
--            @firstNameParser@,
--            @surnameParser@,
--            @smokerParser@,
--            @phoneParser@.
--
-- >>> isErrorResult (parse personParser "")
-- True
--
-- >>> isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Cla y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789")
-- True
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789#"
-- Result >< Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789# rest"
-- Result > rest< Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
personParser ::
  Parser Person
--personParser =
--  error "todo: Course.Parser#personParser"
personParser =
{-  
  flbindParser ageParser (\a ->
  spaces1 >>>
  flbindParser firstNameParser (\f ->
  spaces1 >>>
  flbindParser surnameParser (\s ->
  spaces1 >>>
  flbindParser smokerParser (\g ->
  spaces1 >>>
  flbindParser phoneParser (
  valueParser . Person a f s g)))))
-}

--  (:.) <$> digit <*> phoneBodyParser <* is '#'
--  (:.) <$> ageParser <*> firstNameParser <*> surnameParser <*> smokerParser <*> phoneParser <* is Person
--  (:.) undefined undefined
--  age (call it a)
{-
  do a <- ageParser
  spaces1
--  1/many spaces
--  firstName (call it f)
  f <- firstNameParser
  spaces1
--  1/many spaces
  s <- surnameParser
  spaces1
--  surname (call it s)
--  1/many spaces
  k <- smokerParser
  spaces1
--  smoker (call it s)
--  1/many spaces
  p <- phoneParser
  pure (Person a f s k p)
--  phone (call it s)
--  Person a f s k p
-}
  
  -- > :t Person
--Person :: Int -> Chars -> Chars -> Char -> Chars -> Person

-- :t Person <$> ageParser <*> spaces1 *> firstNameParser <*> surnameParser <*> smokerParser <*> 
  Person <$> ageParser <* spaces1 <*> 
              firstNameParser <* spaces1 <*> 
              surnameParser <* spaces1 <*> 
              smokerParser <* spaces1 <*>  
              phoneParser


-- Make sure all the tests pass!


-- | Write a Functor instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Functor Parser where
  (<$>) ::
    (a -> b)
    -> Parser a
    -> Parser b
--  (<$>) =
--     error "todo: Course.Parser (<$>)#instance Parser"
  (<$>) f =
    bindParser (valueParser . f)


-- | Write a Apply instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Apply Parser where
  (<*>) ::
    Parser (a -> b)
    -> Parser a
    -> Parser b
--  (<*>) =
--    error "todo: Course.Parser (<*>)#instance Parser"
  p <*> q =
    bindParser (\f -> bindParser (valueParser . f) q) p

-- | Write an Applicative functor instance for a @Parser@.
instance Applicative Parser where
  pure ::
    a
    -> Parser a
--  pure =
--    error "todo: Course.Parser pure#instance Parser"
  pure =
    valueParser

-- | Write a Bind instance for a @Parser@.
instance Bind Parser where
  (=<<) ::
    (a -> Parser b)
    -> Parser a
    -> Parser b
--  (=<<) =
--    error "todo: Course.Parser (=<<)#instance Parser"
  (=<<) =
    bindParser

instance Monad Parser where
