module Tokenizer
  ( tokenize
  ) where

import           Data.Char (isAlpha, isAscii, isAsciiLower, isAsciiUpper,
                            isNumber)
import           Token     (DToken (..), TError (..), Token (..), TokenList)

tokenize :: String -> TokenList
tokenize = tok [] 1 1

tok :: [DToken] -> Int -> Int -> String -> TokenList
tok acc l c "" = Right $ reverse $ dec TEoF l c : acc
tok acc l c p@(' ':_) =
  let (c', p') = spaces c p
   in tok (dec TSpace l c : acc) l c' p'
tok acc l c ('\r':'\n':p) = tok (dec TNewline l c : acc) (l + 1) 1 p
tok acc l c ('\n':p) = tok (dec TNewline l c : acc) (l + 1) 1 p
tok acc l c ('\r':p) = tok (dec TNewline l c : acc) (l + 1) 1 p
tok acc l c p@('#':_) =
  let (c', p', s) = comment c p
   in tok (dec (TComment s) l c : acc) l c' p'
tok acc l c ('(':p) = tok (dec TBRacketL l c : acc) l (c + 1) p
tok acc l c (')':p) = tok (dec TBracketR l c : acc) l (c + 1) p
tok acc l c ('{':p) = tok (dec TCurlyL l c : acc) l (c + 1) p
tok acc l c ('}':p) = tok (dec TCurlyR l c : acc) l (c + 1) p
tok acc l c ('[':p) = tok (dec TSquareL l c : acc) l (c + 1) p
tok acc l c (']':p) = tok (dec TSquareR l c : acc) l (c + 1) p
tok acc l c (',':p) = tok (dec TComma l c : acc) l (c + 1) p
tok acc l c (';':p) = tok (dec TSemic l c : acc) l (c + 1) p
tok acc l c ('.':'.':p) = tok (dec TDots l c : acc) l (c + 2) p
tok acc l c ('-':'>':p) = tok (dec TArrow l c : acc) l (c + 2) p
tok acc l c (':':'=':p) = tok (dec TWalrus l c : acc) l (c + 2) p
tok acc l c (':':p) = tok (dec TColon l c : acc) l (c + 1) p
tok acc l c ('+':p) = tok (dec TPlus l c : acc) l (c + 1) p
tok acc l c ('-':p) = tok (dec TMinus l c : acc) l (c + 1) p
tok acc l c ('*':p) = tok (dec TStar l c : acc) l (c + 1) p
tok acc l c ('/':p) = tok (dec TSlash l c : acc) l (c + 1) p
tok acc l c ('=':'>':p) = tok (dec TImplies l c : acc) l (c + 2) p
tok acc l c ('<':'=':'>':p) = tok (dec TImplies l c : acc) l (c + 3) p
tok acc l c ('=':p) = tok (dec TEq l c : acc) l (c + 1) p
tok acc l c ('!':'=':p) = tok (dec TNEq l c : acc) l (c + 2) p
tok acc l c ('<':'=':p) = tok (dec TLEq l c : acc) l (c + 2) p
tok acc l c ('<':p) = tok (dec TLess l c : acc) l (c + 1) p
tok acc l c ('>':'=':p) = tok (dec TGEq l c : acc) l (c + 2) p
tok acc l c ('>':p) = tok (dec TGreater l c : acc) l (c + 1) p
tok acc l c ('!':p) = tok (dec TNot l c : acc) l (c + 1) p
tok acc l c ('|':p) = tok (dec TOr l c : acc) l (c + 1) p
tok acc l c ('&':p) = tok (dec TAnd l c : acc) l (c + 1) p
tok acc l c p@('g':'r':'a':'p':'h':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TGraph l c : acc) l (c + 5) t
tok acc l c p@('v':'a':'r':'i':'a':'b':'l':'e':'s':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TVars l c : acc) l (c + 9) t
tok acc l c p@('s':'t':'a':'t':'e':'s':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TStates l c : acc) l (c + 6) t
tok acc l c p@('i':'n':'i':'t':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TInit l c : acc) l (c + 4) t
tok acc l c p@('t':'r':'a':'n':'s':'i':'t':'i':'o':'n':'s':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TTransitions l c : acc) l (c + 11) t
tok acc l c p@('g':'u':'a':'r':'d':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TGuard l c : acc) l (c + 5) t
tok acc l c p@('a':'c':'t':'i':'o':'n':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TAction l c : acc) l (c + 6) t
tok acc l c p@('b':'o':'o':'l':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TBool l c : acc) l (c + 4) t
tok acc l c p@('i':'n':'t':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TInt l c : acc) l (c + 3) t
tok acc l c p@('e':'n':'u':'m':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TEnum l c : acc) l (c + 4) t
tok acc l c p@('s':'t':'a':'t':'e':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TState l c : acc) l (c + 5) t
tok acc l c p@('t':'r':'u':'e':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TTrue l c : acc) l (c + 4) t
tok acc l c p@('f':'a':'l':'s':'e':t)
  | nextAlphaNum t = tokLower acc l c p
  | otherwise = tok (dec TFalse l c : acc) l (c + 1) t
tok acc l c p@(x:_)
  | isNumber x =
    let (c', p', n) = readNumber c p
     in tok (dec (TNumber n) l c : acc) l c' p'
  | isAsciiUpper x =
    let (c', p', s) = readName c p
     in tok (dec (TUpper s) l c : acc) l c' p'
  | isAsciiLower x = tokLower acc l c p
  | otherwise =
    Left $ TError {tMsg = "Invalid char: " ++ show x, tLine = l, tCol = c}

tokLower :: [DToken] -> Int -> Int -> String -> TokenList
tokLower acc l c p =
  let (c', p', s) = readName c p
   in tok (dec (TLower s) l c : acc) l c' p'

spaces :: Int -> String -> (Int, String)
spaces i (' ':s) = spaces (i + 1) s
spaces i s       = (i, s)

comment :: Int -> String -> (Int, String, String)
comment = comment' ""
  where
    comment' :: String -> Int -> String -> (Int, String, String)
    comment' acc i s@('\r':'\n':_) = (i, s, reverse acc)
    comment' acc i s@('\r':_)      = (i, s, reverse acc)
    comment' acc i s@('\n':_)      = (i, s, reverse acc)
    comment' acc i (c:s)           = comment' (c : acc) (i + 1) s

readNumber :: Int -> String -> (Int, String, Int)
readNumber = readNumber' ""
  where
    readNumber' :: String -> Int -> String -> (Int, String, Int)
    readNumber' acc i p@(x:xs)
      | isNumber x = readNumber' (x : acc) (i + 1) xs
      | otherwise = (i, p, read $ reverse acc)

readName :: Int -> String -> (Int, String, String)
readName = readName' ""
  where
    readName' :: String -> Int -> String -> (Int, String, String)
    readName' acc i p@(x:xs)
      | isAsciiAlphaNum x = readName' (x : acc) (i + 1) xs
      | otherwise = (i, p, reverse acc)

nextAlphaNum :: String -> Bool
nextAlphaNum "" = False
nextAlphaNum s  = isAsciiAlphaNum $ head s

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum x = isAscii x && (isAlpha x || isNumber x)

dec :: Token -> Int -> Int -> DToken
dec t l c = DToken {token = t, line = l, column = c}
