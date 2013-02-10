-- | URI Parser/combiner, adhering to RFC 3986.
--
-- Author     : Alex Stangl
-- License    : BSD

module Database.HSparql.UriParser (
    mergeUris,
    parseWithBaseUriString,
    parseWithBaseUri
	) where

import Control.Monad(liftM,liftM2)
import Data.Char(chr,digitToInt,toLower,toUpper)
import Data.Maybe(isJust)
import Test.HUnit
import Text.ParserCombinators.Parsec


data Authority = Authority {
                     authUserinfo :: Maybe String,
                     authHost :: String,
                     authPort :: Maybe String}
    deriving Eq

instance Show Authority where
    show (Authority ui h p) = let uis = case ui of Nothing -> ""
                                                   Just u -> u ++ "@"
                                  ps = case p of Nothing -> ""
                                                 Just port -> (':':port)
                              in uis ++ h ++ ps

data ParsedUri = ParsedUri {
                     puScheme :: Maybe String,
                     puAuthority :: Maybe Authority,
                     puPath :: String,
                     puQuery :: Maybe String,
                     puFragment :: Maybe String}

instance Show ParsedUri where
    show (ParsedUri s a p q f) =
        let scheme = case s of Nothing -> ""
                               Just sc -> sc ++ ":"
            auth = case a of Nothing -> ""
                             Just au -> "//" ++ show au
            path = p
            query = case q of Nothing -> ""
                              Just qu -> '?' : qu
            fragment = case f of Nothing -> ""
                                 Just fr -> '#' : fr
        in scheme ++ auth ++ path ++ query ++ fragment

                     
-- |Parser that parses absolute or relative URIs.
parseUri :: String -> Either ParseError ParsedUri
parseUri = parse uriReference ""

uri = do
    s <- scheme
    char ':'
    (a,p) <- hierPart
    q <- option Nothing $ liftM Just (char '?' >> query)
    f <- option Nothing $ liftM Just (char '#' >> fragment)
    eof
    return $ ParsedUri (Just s) a p q f

hierPart = do string "//"
              a <- authority
              p <- pathAbempty
              return (Just a,p)
       <|> do p <- pathAbsolute
              return (Nothing,p)
       <|> do p <- pathRootless
              return (Nothing,p)
       <|> do p <- pathEmpty
              return (Nothing,p)

uriReference = try uri <|> relativeRef

relativeRef = do
    (a,p) <- relativePart
    q <- option Nothing $ liftM Just (char '?' >> query)
    f <- option Nothing $ liftM Just (char '#' >> fragment)
    eof
    return $ ParsedUri Nothing a p q f

relativePart = try (do string "//"
                       a <- authority
                       p <- pathAbempty
                       return (Just a,p))
           <|> try (do p <- pathAbsolute
                       return (Nothing,p))
           <|> try (do p <- pathNoscheme
                       return (Nothing,p))
           <|> do p <- pathEmpty
                  return (Nothing,p)

-- |Parse scheme name, return its canonical (lowercase) representation.
scheme = do
    c <- alpha
    cs <- many $ oneOf (['A'..'Z'] ++ ['a'..'z'] ++ "0123456789+-.")
    return $ map toLower (c:cs)

authority :: Parser Authority
authority = do
    ui <- option Nothing (try (do u <- userinfo
                                  char '@'
                                  return $ Just u))
    h <- host
    p <- option Nothing (do char ':'
                            po <- port
                            return $ Just po)
    return $ Authority ui h p

userinfo = liftM concat $ many (charToString unreserved
                            <|> pctEncoded
                            <|> charToString subDelims
                            <|> string ":")

host :: Parser String
host = ipLiteral <|> ipv4address <|> regName

port = many digit

ipLiteral :: Parser String
ipLiteral = between (char '[') (char ']') (ipv6address <|> ipvFuture)

ipvFuture = do char 'v'
               h <- many1 hexDigit
               char '.'
               s <- many1 (unreserved <|> subDelims <|> char ':')
               return (('v':h) ++ "." ++ s)

ipv6address :: Parser String
ipv6address = try (do hs <- count 6 h16Colon
                      s <- ls32
                      return $ concat hs ++ s)
          <|> try (do co <- string "::"
                      hs <- count 5 h16Colon
                      s <- ls32
                      return $ co ++ concat hs ++ s)
          <|> try (do p <- option "" h16
                      co <- string "::"
                      hs <- count 4 h16Colon
                      s <- ls32
                      return $ p ++ co ++ concat hs ++ s)
          <|> try (do ps <- upTo 1 h16Colon
                      pp <- h16
                      co <- string "::"
                      hs <- count 3 h16Colon
                      s <- ls32
                      return $ concat ps ++ pp ++ co ++ concat hs ++ s)
          <|> try (do ps <- upTo 2 h16Colon
                      pp <- h16
                      co <- string "::"
                      hs <- count 2 h16Colon
                      s <- ls32
                      return $ concat ps ++ pp ++ co ++ concat hs ++ s)
          <|> try (do ps <- upTo 3 h16Colon
                      pp <- h16
                      co <- string "::"
                      h <- h16Colon
                      s <- ls32
                      return $ concat ps ++ pp ++ co ++ h ++ s)
          <|> try (do ps <- upTo 4 h16Colon
                      pp <- h16
                      co <- string "::"
                      s <- ls32
                      return $ concat ps ++ pp ++ co ++ s)
          <|> try (do ps <- upTo 5 h16Colon
                      pp <- h16
                      co <- string "::"
                      h <- h16
                      return $ concat ps ++ pp ++ co ++ h)
          <|> try (do ps <- upTo 6 h16Colon
                      pp <- h16
                      co <- string "::"
                      return $ concat ps ++ pp ++ co)

h16 = count 4 hexDigit
ls32 = try (do h1 <- h16
               co <- string ":"
               h2 <- h16
               return $ h1 ++ co ++ h2)
   <|> ipv4address

ipv4address :: Parser String
ipv4address = do o1 <- decOctet
                 char '.'
                 o2 <- decOctet
                 char '.'
                 o3 <- decOctet
                 char '.'
                 o4 <- decOctet
                 return (o1 ++ "." ++ o2 ++ "." ++ o3 ++ "." ++ o4)

decOctet :: Parser String
decOctet = try (do d <- digit
                   return [d])
       <|> try (do c <- oneOf "123456789"
                   d <- digit
                   return (c:d:""))
       <|> try (do char '1'
                   d1 <- digit
                   d2 <- digit
                   return ('1':d1:d2:""))
       <|> try (do char '2'
                   d1 <- oneOf "01234"
                   d2 <- digit
                   return ('2':d1:d2:""))
       <|> do string "25"
              d <- oneOf "012345"
              return ("25" ++ [d])

regName = liftM concat $ many (charToString unreserved
                           <|> pctEncoded
                           <|> charToString subDelims)

pathAbempty = do p <- many (do c <- char '/'
                               s <- segment
                               return (c:s))
                 return $ concat p

pathAbsolute = liftM2 (:) (char '/') (option "" pathRootless)

pathNoscheme = do s <- segmentNzNc
                  ss <- many (liftM2 (:) (char '/') segment)
                  return (s ++ concat ss)

pathRootless = do f <- segmentNz
                  ss <- many (do c <- char '/'
                                 s <- segment
                                 return (c:s))
                  return (f ++ concat ss)

pathEmpty = string ""

segment = liftM concat (many pchar)
segmentNz = liftM concat (many1 pchar)

-- | Parse non-zero-length segment without any colon.
segmentNzNc = liftM concat (many1 (charToString unreserved
                               <|> pctEncoded
                               <|> charToString subDelims
                               <|> string ":"
                               <|> string "@"))

pchar = charToString unreserved
    <|> pctEncoded
    <|> charToString subDelims
    <|> charToString (oneOf ":@")
        
query = liftM concat $ many (pchar <|> charToString(oneOf "/?"))
fragment = liftM concat $ many (pchar <|> charToString(oneOf "/?"))

-- |Parse percent-encoded character. If character is unreserved,
-- decode it immediately; otherwise, leaves it percent-encoded for
-- decoding at some later stage (e.g., so reserved character is not
-- misinterpreted as a delimiter).
pctEncoded = do char '%'
                h1 <- hexDigit
                h2 <- hexDigit
                let c = chr $ 16 * digitToInt h1 + digitToInt h2
                if c `elem` unreservedChars
                    then return (c : "")
                    else return ('%' : toUpper h1 : toUpper h2 : "")

unreserved = oneOf unreservedChars

reserved = genDelims <|> subDelims

genDelims = oneOf ":/?#[]@"

subDelims = oneOf "!$&'()*+,;="


-- Extra helper parsers for terminals defined in RFC 2234, tighter than the built-in Haskell/Parsec definitions.
alpha = satisfy (\c -> c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z')

-- | Process 0..n instances of the specified parser, backtracking on failure.
upTo n p = choice [try (count x p) | x <- [0..n]]

-- | Turn a character parser into a string parser that just parses a 1
-- character string, i.e., just changes its return type.
charToString :: Parser Char -> Parser String
charToString = liftM (:"")

-- | Parse h16 followed by a colon, with no backtracking on failure.
h16Colon = do h <- h16
              c <- string ":"
              return (h ++ c)


-- unreserved URI characters
unreservedChars = ['A'..'Z'] ++ ['a'..'z'] ++ "0123456789-._~"


-- | Parse URI string u in the context of base URI string b.
parseWithBaseUriString :: String -> String -> Either ParseError ParsedUri
parseWithBaseUriString u b =
    case (parseUri b) of Left e -> Left e
                         Right base -> parseWithBaseUri u base

-- | Parse URI string u in the context of base URI b.
parseWithBaseUri :: String -> ParsedUri -> Either ParseError ParsedUri
parseWithBaseUri u b =
    case (parseUri u) of Left e -> Left e
                         Right p -> Right (mergeUris p b)

mergeUris :: ParsedUri -- ^ reference URI
    -> ParsedUri -- ^ Base URI
    -> ParsedUri -- ^ resulting merged URI
mergeUris (ParsedUri s a p q f) (ParsedUri bs ba bp bq bf)
  | isJust s = ParsedUri s a (removeDotSegments p) q f
  | isJust a = ParsedUri bs a (removeDotSegments p) q f
  | p == "" = let q' = if isJust q then q else bq
              in ParsedUri bs ba bp q' f
  | otherwise = let (pf:ps) = p
                in if pf == '/'
                   then ParsedUri bs ba (removeDotSegments p) q f
                   else ParsedUri bs ba (removeDotSegments (merge ba bp p)) q f

-- Merge paths from RFC 3986 5.2.3
merge (Just _) "" p = '/':p
merge _ bp p = (reverse . dropWhile (/= '/') . reverse) bp ++ p


-- | Remove dot segments from URI path, per RFC 3986 sect. 5.2.4
-- Output built up as a stack in os, and finally reversed and concatenated.
removeDotSegments :: String -> String
removeDotSegments s = concat $ reverse $ removeDots s []
    where removeDots ('.':'.':'/':is)     os     = removeDots is os
          removeDots ('.':'/':is)         os     = removeDots is os
          removeDots ('/':'.':'/':is)     os     = removeDots ('/':is) os
          removeDots "/."                 os     = removeDots "/" os
          removeDots ('/':'.':'.':'/':is) (o:os) = removeDots ('/':is) os
          removeDots ('/':'.':'.':'/':is) _      = removeDots ('/':is) []
          removeDots "/.."                (o:os) = removeDots "/" os
          removeDots "/.."                _      = removeDots "/" []
          removeDots "."                  os     = os
          removeDots ".."                 os     = os
          removeDots ""                   os     = os
          removeDots (i:is) out = let (p,s) = break (=='/') is
                                  in removeDots s ((i:p):out)


-- HUnit tests

testBase = "http://a/b/c/d;p?q"

testRel u b e = let r = parseWithBaseUriString u b
                in case r of Left e -> assertFailure ("Got error: " ++ show e)
                             Right p -> assertEqual ("For base " ++ show b ++
                                                     ", URI " ++ show u)
                                                    e (show p)

test1 = TestCase (testRel "g:h" testBase "g:h")
test2 = TestCase (testRel "g" testBase "http://a/b/c/g")
test3 = TestCase (testRel "./g" testBase "http://a/b/c/g")
test4 = TestCase (testRel "g/" testBase "http://a/b/c/g/")
test5 = TestCase (testRel "/g" testBase "http://a/g")
test6 = TestCase (testRel "//g" testBase "http://g")
test7 = TestCase (testRel "?y" testBase "http://a/b/c/d;p?y")
test8 = TestCase (testRel "g?y" testBase "http://a/b/c/g?y")
test9 = TestCase (testRel "#s" testBase "http://a/b/c/d;p?q#s")
test10 = TestCase (testRel "g#s" testBase "http://a/b/c/g#s")
test11 = TestCase (testRel "g?y#s" testBase "http://a/b/c/g?y#s")
test12 = TestCase (testRel ";x" testBase "http://a/b/c/;x")
test13 = TestCase (testRel "g;x" testBase "http://a/b/c/g;x")
test14 = TestCase (testRel "g;x?y#s" testBase "http://a/b/c/g;x?y#s")
test15 = TestCase (testRel "" testBase testBase)
test16 = TestCase (testRel "." testBase "http://a/b/c/")
test17 = TestCase (testRel "./" testBase "http://a/b/c/")
test18 = TestCase (testRel ".." testBase "http://a/b/")
test19 = TestCase (testRel "../" testBase "http://a/b/")
test20 = TestCase (testRel "../g" testBase "http://a/b/g")
test21 = TestCase (testRel "../.." testBase "http://a/")
test22 = TestCase (testRel "../../" testBase "http://a/")
test23 = TestCase (testRel "../../g" testBase "http://a/g")
test24 = TestCase (testRel "../../../g" testBase "http://a/g")
test25 = TestCase (testRel "../../../../g" testBase "http://a/g")

test26 = TestCase (testRel "/./g" testBase "http://a/g")
test27 = TestCase (testRel "/../g" testBase "http://a/g")
test28 = TestCase (testRel "g." testBase "http://a/b/c/g.")
test29 = TestCase (testRel ".g" testBase "http://a/b/c/.g")
test30 = TestCase (testRel "g.." testBase "http://a/b/c/g..")
test31 = TestCase (testRel "..g" testBase "http://a/b/c/..g")
test32 = TestCase (testRel "./../g" testBase "http://a/b/g")
test33 = TestCase (testRel "./g/." testBase "http://a/b/c/g/")
test34 = TestCase (testRel "g/./h" testBase "http://a/b/c/g/h")
test35 = TestCase (testRel "g/../h" testBase "http://a/b/c/h")
test36 = TestCase (testRel "g;x=1/./y" testBase "http://a/b/c/g;x=1/y")
test37 = TestCase (testRel "g;x=1/../y" testBase "http://a/b/c/y")
test38 = TestCase (testRel "g?y/./x" testBase "http://a/b/c/g?y/./x")
test39 = TestCase (testRel "g?y/../x" testBase "http://a/b/c/g?y/../x")
test40 = TestCase (testRel "g#s/./x" testBase "http://a/b/c/g#s/./x")
test41 = TestCase (testRel "g#s/../x" testBase "http://a/b/c/g#s/../x")
allTests = TestList [test1,test2,test3,test4,test5,
                     test6,test7,test8,test9,test10,
                     test11,test12,test13,test14,test15,
                     test16,test17,test18,test19,test20,
                     test21,test22,test23,test24,test25,
                     test26,test27,test28,test29,test30,
                     test31,test32,test33,test34,test35,
                     test36,test37,test38,test39,test40,
                     test41]

-- |Test runner to run all HUnit tests.
runTestAll = runTestTT allTests

